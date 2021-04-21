tfp = reticulate::import("tensorflow_probability")
library(tensorflow)


tfd = tfp$distributions
psd_kernels = tfp$positive_semidefinite_kernels

n = 1000L
observation_noise_variance = 0.3
f = function(x) sin(5*x[,1]) * exp(-x[,1]**2)
observation_index_points = matrix(runif(n,-1,1),n,1)
observations = matrix((f(observation_index_points)) + rnorm(n,0.0, sqrt(observation_noise_variance)))
plot(observation_index_points, observations)
index_points = matrix(seq(-1, 1, length.out = 20),20,1)

kernel = psd_kernels$MaternFiveHalves()

gprm = tfd$GaussianProcessRegressionModel(
  kernel = kernel,
  index_points = index_points,
  observation_index_points = observation_index_points,
  observations = observations,
  observation_noise_variance = observation_noise_variance
)
samples = gprm$sample(10)


amplitude = tf$Variable(0.0,dtype = "float64")
length_scale = tf$Variable(0.0,dtype = "float64")
observation_noise_variance = tf$Variable(-5.0,dtype = "float64")

optimizer = tf$keras$optimizers$Adamax(0.05)

weights = c(amplitude, length_scale, observation_noise_variance)

for(i in 1:1000){
  with(tf$GradientTape() %as% tape, {
    tape$watch(amplitude)
    tape$watch(length_scale)
    tape$watch(observation_noise_variance)
    
    kernel = psd_kernels$ExponentiatedQuadratic(tf$exp(amplitude), tf$exp(length_scale))
    gp = tfd$GaussianProcess(
      kernel = kernel,
      index_points = observation_index_points,
      observation_noise_variance = tf$exp(observation_noise_variance))
    
    loss = tf$reduce_mean(-gp$log_prob(observations[,1]))
  })
  grads = tape$gradient(loss, weights)
  optimizer$apply_gradients(purrr::transpose(list(grads, weights)))
  if(i %% 100 == 0) cat("Step: ", i, " Loss: ", loss$numpy(), "\n")
}

samples = gp$sample(10L)$numpy()
plot(observation_index_points[,1], observations)
#points(observation_index_points, samples[2,], col = "red")
quant = apply(samples, 2, quantile, c(0.25,0.5, 0.75))
lines(smooth.spline(x = observation_index_points, quant[1,]))
lines(smooth.spline(x = observation_index_points, quant[2,]))
lines(smooth.spline(x = observation_index_points, quant[3,]))
library(mlegp)

m = mlegp(observation_index_points, observations)

plot(observations,quant[2,])
sqrt(mean((observations - quant[2,])^2))

     