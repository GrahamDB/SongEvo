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

iteration <- 1
timestep <- 1
terr.turnover <- 0.5
integrate.dist <- 0.1
lifespan <- NA
mate.comp <- FALSE
prin <- FALSE

std.args <- list(init.inds = init.inds,
                  females = 1.0,
                  iteration = iteration, 
                  # steps = years,  
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
                  prin = prin)

default.names = c("summary.results","inds","females","content.bias.info","time")
sparse.names = c("summary.results","inds.last","inds.init","inds.slices",
                 "females.last","females.init","females.slices",
                 "content.bias.info","time")
full.names = c("summary.results","inds","all.inds","females","all.females","content.bias.info","time")
# Various tests
test_that("Simple checks of starting parameters, single iteration mode, single step",{
  expect_named( do.call(SongEvo, c(std.args, list(steps=1, all=FALSE))),default.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=1, all="sparse"))),sparse.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=1, all=TRUE))),full.names) 
})
# test_that("Simple checks of starting parameters, single iteration mode, no step",{
#   skip("0 step feature not implemented.")
#   expect_named( do.call(SongEvo, c(std.args, list(steps=0, all=FALSE))),default.names)
#   expect_named( do.call(SongEvo, c(std.args, list(steps=0, all="sparse"))),sparse.names)
#   expect_named( do.call(SongEvo, c(std.args, list(steps=0, all=TRUE))),full.names)
# })
test_that("Simple checks of starting parameters, single iteration mode, 2 step",{
  expect_named( do.call(SongEvo, c(std.args, list(steps=2, all=FALSE))),default.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=2, all="sparse"))),sparse.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=2, all=TRUE))),full.names)
})

std.args$iteration=2
test_that("Simple checks of starting parameters, multi iteration mode, single step",{
  expect_named( do.call(SongEvo, c(std.args, list(steps=1, all=FALSE))),default.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=1, all="sparse"))),sparse.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=1, all=TRUE))),full.names)
})
# test_that("Simple checks of starting parameters, multi iteration mode, no step",{
#   skip("0 step feature not implemented.")
#   expect_named( do.call(SongEvo, c(std.args, list(steps=0, all=FALSE))),default.names)
#   expect_named( do.call(SongEvo, c(std.args, list(steps=0, all="sparse"))),sparse.names)
#   expect_named( do.call(SongEvo, c(std.args, list(steps=0, all=TRUE))),full.names)
# })
test_that("Simple checks of starting parameters, multi iteration mode, 2 step",{
  expect_named( do.call(SongEvo, c(std.args, list(steps=2, all=FALSE))),default.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=2, all="sparse"))),sparse.names)
  expect_named( do.call(SongEvo, c(std.args, list(steps=2, all=TRUE))),full.names)
})
