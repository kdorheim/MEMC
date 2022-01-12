# Typically this script should be run with the current developmental version of the branch.
devtools::load_all()
# Otherwise will need to load the MEMC package
# library(MEMC)

# The basic MEND 2013 model
MEND_model <- configure_model(params = default_params, state = default_inital,
                              carbon_pools_func = carbon_pools,
                              carbon_fluxes_func = carbon_fluxes, name = "MEND")
usethis::use_data(MEND_model, overwrite = TRUE, internal = FALSE)


# This is just an example need to be modified a bit more...
comission_carbon_fluxes <- modify_fluxes_func(params = default_params,
                                              state = default_inital,
                                              flux_func = carbon_fluxes,
                                              replace_with = select_F1(kinetics = "RMM"))
COMISSION_model <- configure_model(params = default_params,
                                   state = default_inital,
                                   carbon_pools_func = carbon_pools,
                                   carbon_fluxes_func = comission_carbon_fluxes,
                                   name = "COMISSION")

solve_model(COMISSION_model,time = 1:5 )
usethis::use_data(COMISSION_model, overwrite = TRUE, internal = FALSE)


