### See vignette for an example that uses all functions in SongEvo.

#### Specify and call `par.sens()`

# Here we test the sensitivity of the Acquire a Territory submodel to variation
# in territory turnover rates, ranging from 0.8–1.2 times the published rate
# (40–60% of territories turned over). The call for the par.sens function has a
# format similar to SongEvo. The user specifies the parameter to test and the
# range of values for that parameter. The function currently allows examination
# of only one parameter at a time and requires at least two iterations.
parm <- "terr.turnover"
par.range = seq(from=0.45, to=0.55, by=0.05)
sens.results <- NULL
data("song.data")
data("glo.parms")

# Hack to use glo.parms from SongEvo v1:
glo.parms$mortality.a.m <- glo.parms$mortality.a.f <- glo.parms$mortality.a
glo.parms$mortality.j.m <- glo.parms$mortality.j.f <- glo.parms$mortality.j
glo.parms <- glo.parms[!names(glo.parms) %in% c("mortality.a","mortality.j")]

years=2005-1969
iteration=5
timestep=1
n.territories <- glo.parms$n.territories
starting.trait <- subset(song.data, Population=="Bear Valley" & Year==1969)$Trill.FBW
starting.trait2 <- c(starting.trait, rnorm(n.territories-length(starting.trait),
                                           mean=mean(starting.trait), sd=sd(starting.trait)))
init.inds <- data.frame(id = seq(1:n.territories), age = 2, trait = starting.trait2)
init.inds$x1 <-  round(runif(n.territories, min=-122.481858, max=-122.447270), digits=8)
init.inds$y1 <-  round(runif(n.territories, min=37.787768, max=37.805645), digits=8)
# Now we call the par.sens function with our specifications.
extra_parms <- list(init.inds = init.inds, 
                    females = 1,  # New in SongEvo v2
                    timestep = 1, 
                    n.territories = nrow(init.inds),
                    integrate.dist = 0.1,
                    lifespan = NA, 
                    terr.turnover = 0.5, 
                    mate.comp = FALSE, 
                    prin = FALSE,
                    all = TRUE,
                    # New in SongEvo v2
                    selectivity = 3,
                    content.bias = FALSE,
                    n.content.bias.loc = "all",
                    content.bias.loc = FALSE,
                    content.bias.loc.ranges = FALSE,
                    affected.traits = FALSE,
                    conformity.bias = FALSE,
                    prestige.bias=FALSE,
                    learn.m="default",
                    learn.f="default",
                    learning.error.d=0,
                    learning.error.sd=200)
global_parms_key <- which(!names(glo.parms) %in% names(extra_parms))
extra_parms[names(glo.parms[global_parms_key])]=glo.parms[global_parms_key]
test_that("Running par.sens", {
  expect_type(par.sens1 <- par.sens(parm = parm, par.range = par.range, 
                      iteration = iteration, steps = years, mate.comp = FALSE, 
                      fixed_parms=extra_parms[names(extra_parms)!=parm], all = TRUE), "list")
})

  
#### Examine par.sens results
# Examine results objects, which include two arrays: 

# The first array, `sens.results`, contains the SongEvo model results for each
# parameter. It has the following dimensions:
# dimnames(par.sens1$sens.results)

# The second array, `sens.results.diff` contains the quantile range of trait
# values across iterations within a parameter value. It has the following
# dimensions:
# dimnames(par.sens1$sens.results.diff)

