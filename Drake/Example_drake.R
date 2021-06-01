library(drake)
library(dplyr)
library(ggplot2)
library(tidyr)

create_plot <- function(data) {
  ggplot(data) +
    geom_histogram(aes(x = Ozone)) +
    theme_gray(24)
}

## Download the example data from drake 
drake_example("main", 
              to = paste0(getwd(),"/Drake"))

## check if files are there 
file.exists("Drake/main/raw_data.xlsx")

file.exists("Drake/main/report.Rmd")

#### Setting the stage #####

## provide a plan, what you are actually doing

plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("Drake/main/raw_data.xlsx")),
  data = raw_data %>%
    mutate(Ozone = replace_na(Ozone, mean(Ozone, na.rm = TRUE))),
  hist = create_plot(data),
  fit = lm(Ozone ~ Wind + Temp, data),
  report = rmarkdown::render(
    knitr_in("Drake/main/report.Rmd"),
    output_dir = paste0(getwd(),"/Drake/main"),
    output_file = file_out("report.html"),
    quiet = F
  )
)


## execute the code
make(plan)

## reports are stored in hidden drake folder 

drake::readd(data) 
drake::readd(hist)

## lets fix the plotting function 

create_plot <- function(data) {
  ggplot(data) +
    geom_histogram(aes(x = Ozone), binwidth = 10) +
    theme_gray(24)
}

## check what has to be redone 

library(visNetwork)
vis_drake_graph(plan)

## reproducability

outdated(plan)


### use 

clean()    # Remove the original author's results.
make(plan)


### Big data efficiency 
library(fst)
n <- 1e6 # Each target is 1.6 GB in memory.
plan <- drake_plan(
  data_fst = target(
    data.frame(x = runif(n), y = runif(n)),
    format = "fst"
  ),
  data_old = data.frame(x = runif(n), y = runif(n))
)
make(plan)
build_times(type = "build")

## ease of parallelisation 

make(plan, jobs = 4)

# Or scale up to a supercomputer.
drake_hpc_template_file("slurm_clustermq.tmpl") # https://slurm.schedmd.com/
options(
  clustermq.scheduler = "clustermq",
  clustermq.template = "slurm_clustermq.tmpl"
)
make(plan, parallelism = "clustermq", jobs = 4)