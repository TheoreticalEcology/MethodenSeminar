#' dosomething for parallel
#'
#'@param vector loop over
#'@param time time delay
#'@export

dosomething = function(vector = 1:1e3, time = 0.1){

  print(system.time({dosomething_(vector, time)}))
}


#' distance func
#' @param m
#' @param parallel
#' @export
distance = function(m = matrix(1,10,10), parallel = TRUE){
  if(!parallel) distance_(m)
  else distance_parallel_(m)
}

# size = 80000
# dosomething(1:size)
#
# system.time({
# vector = 1:size
# out = rep(0, size)
# for(i in 1:size){
#   out[i] = atan(vector[i]*vector[i] / vector[i] ) / (vector[i] + vector[i])
# }
# })

# for(int i = 0; i < (size); i++){
#   out[i]= atan(x[i]*x[i] / x[i] ) / (x[i] + x[i]);
#   Rcout  << i << "\n"<< std::endl;
# }
