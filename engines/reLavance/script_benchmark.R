#
# Beating the Benchmark 
# Search Results Relevance @ Kaggle
# __author__ : Abhishek
# __ported to R__:gmilosev
# Tested on MS Windows Server 2012, Quad Core Xeon 3.2 Ghz, 24 Gb RAM
# R Version 3.2.0 64 bit
#

library("tm")
library("caret");
library("Metrics")
library("e1071");
library("rARPACK");
library("RWeka");
library("Matrix");
library("kernlab")
library("readr")
library("slam");
library("doParallel");
library("foreach");

python.tfidf = function(txt,smooth_idf=T,sublinear_tf=F,
                        normf=NULL,
                        min_df=1,
                        do.trace=T,
                        use_idf = T,
                        ngram_range=NULL){
  #corpus first
  corp = NULL;
  if(do.trace) print("Building corpus!");
  if (!is.na(match(class(txt),c("VCorpus","Corpus")))) corp = txt;
  if (!is.na(match(class(txt),c("VectorSource","SimpleSource","Source")))) {corp = Corpus(txt);}
  if (class(txt) == "character") {corp = Corpus(VectorSource(txt));}
  if (is.null(corp)) {stop(paste("Don't know what to do with", class(txt)));}
  #document term matrix
  if(do.trace) print("Building document term matrix!");
  #ngram range
  Tokenizer = NULL;
  if (!is.null(ngram_range)){
    if(do.trace) print(paste("Using NGramTokenizer, range:",ngram_range[1],":",ngram_range[2]));
    Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram_range[1], max = ngram_range[2]))
    options(mc.cores=1)
    dtm = DocumentTermMatrix(corp,control=list(tokenize=Tokenizer,removePunctuation=T,wordLengths=c(1,10000),weighting=function(x) weightTf(x)))
  } else {
    dtm = DocumentTermMatrix(corp,control=list(removePunctuation=T,wordLengths=c(1,10000),weighting=function(x) weightTf(x)));
  }
  if(do.trace) print("Converting from sparse to dense matrix!");
  m = dtm;
  # When building the vocabulary ignore terms that have a document frequency
  # strictly lower than the given threshold.
  # This value is also called cut-off in the literature.
  cs = col_sums(m>0);
  n_doc = dim(m)[1];
  if(do.trace) print("Removing sparse terms!");
  
  # drop the terms with less than min_df freq
  # hard min freq integer
  if (min_df %% 1 == 0){
    m = m[,cs>=min_df];
    cs = col_sums(m>0);
    
    if(do.trace) print(paste("TDM dim:",dim(m)[1],":",dim(m)[2]));
  } else {
    #consider it as percentage
    if (min_df > 0 && min_df <= 1){
      thr = n_doc * (1-min_df);
      if (thr < 1) thr = 1;
      m = m[,cs>=thr];
      cs = col_sums(m>0);
    } else {
      stop("Don't know what to do with min_df, not an integer and not a float 0..1");
    }
  }
  
  # Apply sublinear tf scaling, i.e. replace tf with 1 + log(tf).
  if(sublinear_tf==TRUE) {
    if(do.trace) print("Applying sublinear tf scaling!");
    m$v = 1 + log(m$v);
  }
  
  # Smooth idf weights by adding one to document frequencies, as if an
  # extra document was seen containing every term in the collection
  # exactly once. Prevents zero divisions.
  if(smooth_idf==TRUE) {
    if(do.trace) print("Applying idf smoothing!");
    n_doc = n_doc + 1;
    cs = cs + 1;
  }
  
  # cast to sparse matrix
  # so that Diagonal * m is fast and eficient
  m = sparseMatrix(m$i,m$j,x=m$v,dims=dim(m),dimnames=dimnames(m));
  if (use_idf){
    idf = 1+log(n_doc/cs);
    d = Diagonal(length(idf),idf);
    m = m%*%d;
    d = NULL;
  }
  
  if (is.null(normf)) normf="";
  
  # normalize L1 or L2 
  if (!is.na(match(normf,c("l1","l2")))) {
    if(do.trace) print(paste("Applying",normf,"normalization!"));
    l_m = m;
    if (normf=="l2"){
      l_m@x = l_m@x^2;
      rs = sqrt(rowSums(l_m));
    } else {
      l_m@x = abs(l_m@x);
      rs = rowSums(l_m);
    }
    #avoid division by zero
    rs[rs==0] = 1;
    m = m / rs;
  }
  
  # return sparse matreix
  if(do.trace) print("Done!");
  return(m);
}

train = read_csv("../input/train.csv");
test = read_csv("../input/test.csv");

y = train$median_relevance;
y_var = train$relevance_variance;
queries = as.factor(train$query);

# combine query title and description into single character array
txt = paste(train$query,train$product_title);
txt = c(txt,paste(test$query,test$product_title));

# get document term matrix
m = python.tfidf(txt,sublinear=T,smooth_idf=T,normf="l2",min_df=3,ngram_range=c(1,5));

# split to train and test
train.data = m[1:nrow(train),];
test.data = m[(nrow(train)+1):nrow(m),];

# cv_fold_count tells the script how much 
# folds to use to estimate kappa metric
cv_fold_count = 2;

# parallel infrastructure
n.cores = detectCores();
cl <- makeCluster(n.cores); 
registerDoParallel(cl);

# tuning grid, change ncomp and cost

# -- change the line below to something like tune.grid = expand.grid(ncomp=c(300,400),cost=c(10,12));

tune.grid = expand.grid(ncomp=c(100),cost=c(1,2));

# run grid search in parallel
results = 
    foreach(gridId = c(1:nrow(tune.grid)), .packages=c('kernlab','rARPACK','caret','Metrics','Matrix'), .combine=rbind, .multicombine=T) %dopar% {
      set.seed(2603); #-> so it can be compared when using similar comps, e.g. 10,11,12
      
      # stratified folds by queries so each fold has appox same query dist.
      folds = createFolds(as.factor(y),cv_fold_count);
      svm_cost = tune.grid[gridId,"cost"];
      svd_ncomp = tune.grid[gridId,"ncomp"];
      q.kappa = 0;
                            
      # do the folds
      for(i in 1: length(folds)){
        # get the sampling
        # and construct train and test matrix out of train data
        smpl = folds[[i]];
        g_train = train.data[-smpl,];
        g_test = train.data[smpl,];
        y_train = y[-smpl];
        y_test = y[smpl];

        #svd here
        g_train_svd = svds(g_train,k = svd_ncomp,nv =svd_ncomp, nu=0);
                              
        # note that u must multiply svd$v matrix with train and 
        # test matrix.
        g_train = g_train%*%g_train_svd$v;
        g_test = g_test%*%g_train_svd$v;
                              
        # train the svm, I'm using kernlab but e1071 svm is also good
        # first one seems to give better results for same hyper params
        sv = ksvm(as.matrix(g_train),as.factor(y_train),kernel="rbfdot",C=svm_cost,scaled=T,kpar=list("sigma"=1/dim(g_train)[2]));
        p = predict(sv,newdata=g_test,type="response");
                              
        # calc the quadratic kappa
        q.kappa = q.kappa + ScoreQuadraticWeightedKappa(as.numeric(as.character(y_test)),as.numeric(as.character(p)));
      }
      return (c(
              "qkapp"=q.kappa/length(folds),
              "svd_ncomp"=svd_ncomp,
              "svm_cost"=svm_cost
            ));
}
stopCluster(cl);
                        
# get best results
results = data.frame(results,row.names=NULL);
print(results);

best_result = (results[order(results$qkapp,decreasing=T),])[1,];
best_result;

# svd 
g_train_svd = svds(train.data,k = best_result$svd_ncomp,nv =best_result$svd_ncomp, nu=0);

train.data.fin = train.data %*% g_train_svd$v;
test.data.fin = test.data %*% g_train_svd$v;

# train best model
# note that for gamma/sigma (same stuff here)
# python gamma by default is 1/ncol
# while R ksvm is using more complex estimation of sigma
# here it is set to python default
# but try ksvm default :)
sv_model = ksvm(as.matrix(train.data.fin),as.factor(y),
          kernel="rbfdot",
          C=best_result$svm_cost,
          scaled=T,
          kpar=list("sigma"=1/dim(train.data.fin)[2]));
sub.p = predict(sv_model,newdata=test.data.fin,type="response");

table(sub.p);

#submission
write.csv(data.frame("id"=test$id, "prediction"=sub.p),"submission_r.csv",quote=F,row.names=F);


