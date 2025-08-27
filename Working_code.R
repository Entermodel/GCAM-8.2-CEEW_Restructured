#Installing packages
install.packages("renv")
renv::init(bare = TRUE)
renv::install("renv@1.0.7")
renv::restore()
write('PATH="${RTOOLS44_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)

#Initialization of GCAM

library(devtools)
load_all()

x <- driver(stop_before = "module_india_LA154.Transport")

#If any changes are required in the codes or to explore how each command in a chunk is working, enter debugg mode
#1 Run the driver before that chunk to generate the input filesx
#driver loads the files into the variable 'x'
x <- driver(stop_before = "module_energy_L242.building_agg")

?#2 To enter debugg mode, run the below written commands together
debug(gcamdata:::module_energy_L242.building_agg)
gcamdata:::module_energy_L242.building_agg("MAKE",x)

# After entering the debugg mode of any chunk, first run the all data command so that it can create a list to load all the required inputs
# Then follow the code from loading inputs, running the fuctions and producing the outputs


# Producing base xmls for running the model
# Sequence of xmls is not important but all are required to run the model


a <- driver(stop_after = "module_batch_transportation_India_xml") #fine


devtools::load_all(".")
driver_drake()
