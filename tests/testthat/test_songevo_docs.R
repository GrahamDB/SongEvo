### See vignette for an example that uses all functions in SongEvo.

### Load the example data: song.data

# To explore the SongEvo package, we will use a database of songs from Nuttall’s
# white-crowned sparrow (*Zonotrichia leucophrys nuttalli*) recorded at three
# locations in 1969 and 2005.

data("song.data")
data("glo.parms")
list2env(glo.parms, globalenv())

### Simulate bird song evolution with `SongEvo()`

#### Define initial individuals

starting.trait <- subset(song.data, Population=="PRBO" & Year==1969)$Trill.FBW

# We want a starting population of 40 individuals, so we generate additional
# trait values to complement those from the existing 30 individuals. Then we
# create a data frame that includes a row for each individual; we add
# identification numbers, ages, and geographical coordinates for each
# individual.
starting.trait2 <- c(starting.trait, rnorm(n.territories-length(starting.trait), 
                                           mean=mean(starting.trait), 
                                           sd=sd(starting.trait)))
init.inds    <- data.frame(id = seq(1:n.territories), age = 2, trait = starting.trait2)
init.inds$x1 <- round(runif(n.territories, min=-122.481858, max=-122.447270), digits=8)
init.inds$y1 <- round(runif(n.territories, min=37.787768, max=37.805645), digits=8)

#### Specify and call the SongEvo model

iteration <- 5
years <- 36
timestep <- 1
terr.turnover <- 0.5
integrate.dist <- 0.1
lifespan <- NA
mate.comp <- FALSE
prin <- FALSE
all <- TRUE

# Now we call SongEvo with our specifications and save it in an object called
# SongEvo1.
SongEvo1 <- SongEvo(init.inds = init.inds,
                    females = 1.0,
                    iteration = iteration, 
                    steps = years,  
                    timestep = timestep, 
                    n.territories = n.territories, 
                    terr.turnover = terr.turnover, 
                    integrate.dist = integrate.dist, 
                    learning.error.d = learning.error.d, 
                    learning.error.sd = learning.error.sd, 
                    mortality.a.m = mortality.a, 
                    mortality.j.m = mortality.j, 
                    lifespan = lifespan, 
                    phys.lim.min = phys.lim.min, 
                    phys.lim.max = phys.lim.max, 
                    male.fledge.n.mean = male.fledge.n.mean, 
                    male.fledge.n.sd = male.fledge.n.sd, 
                    male.fledge.n = male.fledge.n, 
                    disp.age = disp.age, 
                    disp.distance.mean = disp.distance.mean, 
                    disp.distance.sd = disp.distance.sd, 
                    mate.comp = mate.comp, 
                    prin = prin, 
                    all = all)

#### Examine results from SongEvo model

test_that("Naming structure of example SongEvo result", {
  expect_type(SongEvo1, "list")
  expect_named(SongEvo1, c("summary.results", "inds", "all.inds", "females", "all.females", "content.bias.info", "time"))
})

# The model required the following time to run on your computer:
test_that("Time array for example SongEvo result", {
  expect_type(SongEvo1$time, "double")
})

# Three main objects hold data regarding the SongEvo model.  Additional objects
# are used temporarily within modules of the model.

test_that("Expected data.frame objects for example SongEvo result", {
  expect_s3_class(SongEvo1$inds, "data.frame")
  expect_s3_class(SongEvo1$females, "data.frame")
  expect_s3_class(SongEvo1$all.inds, "data.frame")
  expect_s3_class(SongEvo1$all.females, "data.frame")
  expect_true(all(SongEvo1$inds$sex=="M"))
  expect_true(all(SongEvo1$all.inds$sex=="M"))
  expect_true(all(SongEvo1$females$sex=="F"))
  expect_true(all(SongEvo1$all.females$sex=="F"))
})
# Second, an array (i.e. a multi-dimensional table) entitled “summary.results”
# includes population summary values for each time step (dimension 1) in each
# iteration (dimension 2) of the model.  Population summary values are contained
# in five additional dimensions: population size for each time step of each
# iteration (“sample.n”), the population mean and variance of the song feature
# studied (“trait.pop.mean” and “trait.pop.variance”), with associated lower
# (“lci”) and upper (“uci”) confidence intervals.

test_that("Expected summary results array for example SongEvo result", {
  expect_length(dim(SongEvo1$summary.results),3)
  expect_length(dimnames(SongEvo1$summary.results)[[1]],years+1)
  expect_length(dimnames(SongEvo1$summary.results)[[2]],iteration)
  expect_length(dimnames(SongEvo1$summary.results)[[3]],5)
  expect_equal(dimnames(SongEvo1$summary.results)[[3]],c("sample.n","trait.pop.mean","trait.pop.variance","lci","uci"))
  
  expect_false(any(is.na(SongEvo1$summary.results[,,"sample.n"])))
  expect_false(any(is.na(SongEvo1$summary.results[,,"trait.pop.mean"])))
  expect_false(any(is.na(SongEvo1$summary.results[,,"trait.pop.variance"])))
  expect_false(any(is.na(SongEvo1$summary.results[,,"lci"])))
  expect_false(any(is.na(SongEvo1$summary.results[,,"uci"])))
})
