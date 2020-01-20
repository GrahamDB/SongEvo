### See vignette for an example that uses all functions in SongEvo.

#Parameterize SongEvo with initial song data from Schooner Bay, CA in 1969, and
#then compare simulated data to target (i.e. observed) data in 2005.

test_that("Running mod.val", {
data("song.data")
data("glo.parms")

list2env(glo.parms, globalenv()) 
#Prepare initial song data for Schooner Bay.
starting.trait <- subset(song.data, Population=="Schooner" & Year==1969)$Trill.FBW
starting.trait2 <- c(starting.trait, rnorm(n.territories-length(starting.trait), 
                                           mean=mean(starting.trait), 
                                           sd=sd(starting.trait)))
init.inds <- data.frame(id = seq(1:n.territories), age = 2, trait = starting.trait2)
init.inds$x1 <-  round(runif(n.territories, min=-122.481858, max=-122.447270), digits=8)
init.inds$y1 <-  round(runif(n.territories, min=37.787768, max=37.805645), digits=8)

#Specify and call SongEvo() with validation data
iteration <- 5
years <- 36
timestep <- 1
terr.turnover <- 0.5
expect_type(SongEvo2 <- SongEvo(init.inds = init.inds,
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
                    mate.comp = TRUE, 
                    prin = FALSE,
                    all=FALSE), "list")

#Specify and call mod.val
ts <- 36
target.data <- subset(song.data, Population=="Schooner" & Year==2005)$Trill.FBW
expect_type(
mod.val1 <- mod.val(summary.results=SongEvo2$summary.results, ts=ts, target.data=target.data), "list")

})