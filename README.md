# cl-faiss

This is a very tiny wrapper around faiss, a library for k-NN queries. Currently,
only a small fraction of faiss functionality is supported: you can create
indices (only floating point ones), train them, add vectors to them and perform
search queries.

API consists of the following types:

`metric`
`index`

and the following functions:

`index-vector-length`
`index-trained-p`
`index-ntotal`
`index-metric`
`index-train`
`index-add`
`index-search`

An index can be created with `with-index` macro.
