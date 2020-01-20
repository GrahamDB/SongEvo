### See vignette for an example that uses all functions in SongEvo.

test_that("Running SongEvo, then h.test", {
#Prepare initial song data for Bear Valley.
data("song.data")
data("glo.parms")
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

#Specify and call SongEvo() with test data
expect_type(
SongEvo3 <- with(glo.parms,SongEvo(init.inds = init.inds,
                    iteration = iteration,
                    steps = years,
                    timestep = timestep,
                    n.territories = n.territories,
                    terr.turnover = terr.turnover,
                    integrate.dist = 50,
                    learning.error.d = learning.error.d,
                    learning.error.sd = learning.error.sd,
                    mortality.a.m = mortality.a,
                    mortality.j.m = mortality.j,
                    lifespan = NA,
                    phys.lim.min = phys.lim.min,
                    phys.lim.max = phys.lim.max,
                    male.fledge.n.mean = male.fledge.n.mean,
                    male.fledge.n.sd = male.fledge.n.sd,
                    male.fledge.n = male.fledge.n,
                    disp.age = disp.age,
                    disp.distance.mean = disp.distance.mean,
                    disp.distance.sd = disp.distance.sd,
                    mate.comp = FALSE,
                    prin = FALSE,
                    all = FALSE)), "list")

#Specify and call `h.test()`
ts=years
target.data <- subset(song.data, Population=="Bear Valley" & Year==2005)$Trill.FBW
h.test1 <- h.test(summary.results=SongEvo3$summary.results, ts=ts, target.data=target.data)

# The output data list includes two measures of accuracy: the proportion of
# observed points that fall within the confidence intervals of the simulated
# data and the residuals between simulated and observed population trait means.
# Precision is measured as the residuals between simulated and observed
# population trait variances.

# Eighty percent of the observed data fell within the central 95% of the
# simulated values, providing support for the hypothesis that cultural drift as
# described in this model is sufficient to describe the evolution of trill
# frequency bandwidth in this population.
#h.test1
})
