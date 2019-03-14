# TweeterSentiments
I have chosen the brand Brexit which was also termed as the word of the year. We have collected 1000 tweets regarding the same and below is our sentiment Analysis

<<TermDocumentMatrix (terms: 2451, documents: 1000)>>
Non-/sparse entries: 10309/2440691
Sparsity           : 100%
Maximal term length: 20
Weighting          : term frequency (tf)

idx <- which(dimnames(tdmatrix)$Terms %in% c("brexit", "eu"))
as.matrix(tdmatrix[idx,21:60])
> as.matrix(tdmatrix[idx,21:60])
        Docs
Terms    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
  brexit  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1
  eu      0  0  0  1  0  1  0  1  0  0  1  0  1  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0
        Docs
Terms    51 52 53 54 55 56 57 58 59 60
  brexit  1  1  1  1  1  1  2  1  1  1
  eu      0  0  0  0  0  0  0  0  0  0

After polarity and analysis we come to a conclusion
 

Interpretation : We can see that there are more negative terms but the positive sentiments have greater contributions.
