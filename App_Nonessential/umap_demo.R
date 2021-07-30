library(uwot)

# See function man page for help
?umap

# Non-numeric columns are ignored, so in a lot of cases you can pass a data
# frame directly to umap
iris_umap <- umap(iris, n_neighbors = 50, learning_rate = 0.5, init = "random")

# Load mnist from somewhere, e.g.
#devtools::install_github("jlmelville/snedata")
mnist <- snedata::download_mnist()
mnist_umap <- umap(mnist, n_neighbors = 15, min_dist = 0.001, verbose = TRUE)

# For high dimensional datasets (> 100-1000 columns) using PCA to reduce
# dimensionality is highly recommended to avoid the nearest neighbor search
# taking a long time. Keeping only 50 dimensions can speed up calculations
# without affecting the visualization much
mnist_umap <- umap(mnist, pca = 50)

# Use a specific number of threads
mnist_umap <- umap(mnist, n_neighbors = 15, min_dist = 0.001, verbose = TRUE, n_threads = 8)

# Use a different metric
mnist_umap_cosine <- umap(mnist, n_neighbors = 15, metric = "cosine", min_dist = 0.001, verbose = TRUE, n_threads = 8)

# If you are only interested in visualization, `fast_sgd = TRUE` gives a much faster optimization
mnist_umap_fast_sgd <- umap(mnist, n_neighbors = 15, metric = "cosine", min_dist = 0.001, verbose = TRUE, fast_sgd = TRUE)

# Supervised dimension reduction
mnist_umap_s <- umap(mnist, n_neighbors = 15, min_dist = 0.001, verbose = TRUE, n_threads = 8,
                     y = mnist$Label, target_weight = 0.5)

# Add new points to an existing embedding
mnist_train <- head(mnist, 60000)
mnist_test <- tail(mnist, 10000)

# You must set ret_model = TRUE to return extra data we need
# coordinates are in mnist_train_umap$embedding
mnist_train_umap <- umap(mnist_train, verbose = TRUE, ret_model = TRUE)
mnist_test_umap <- umap_transform(mnist_test, mnist_train_umap, verbose = TRUE)

# Save the nearest neighbor data
mnist_nn <- umap(mnist, ret_nn = TRUE)
# coordinates are now in mnist_nn$embedding

# Re-use the nearest neighor data and save a lot of time
mnist_nn_spca <- umap(mnist, nn_method = mnist_nn$nn, init = spca)

# No problem to have ret_nn = TRUE and ret_model = TRUE at the same time
# Or just use the ret_extra parameter:
mnist_nn_and_model <- umap(mnist, ret_extra = c("model", "nn"))

# You can also get to the input fuzzy graph as a sparse matrix via "fgraph"
mnist_with_fgraph <- umap(mnist, ret_extra = c("fgraph"))
# equivalent for lvish is to use "P" (input probability matrix):
mnist_with_P <- lvish(mnist, ret_extra = c("P"))

# Calculate Petal and Sepal neighbors separately (uses intersection of the resulting sets):
iris_umap <- umap(iris, metric = list("euclidean" = c("Sepal.Length", "Sepal.Width"),
                                      "euclidean" = c("Petal.Length", "Petal.Width")))
# Can also use individual factor columns
iris_umap <- umap(iris, metric = list("euclidean" = c("Sepal.Length", "Sepal.Width"),
                                      "euclidean" = c("Petal.Length", "Petal.Width"),
                                      "categorical" = "Species"))