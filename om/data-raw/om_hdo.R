
library(om)
library(usethis)

om_object <- om(ages = 0:12, samples = 1, time = 0:300)

# get pars
s_pars   <- as.numeric(unlist(logitnorm_pars(0.95, 0.2)[1:2]))
b_pars   <- c(9.61, 13.86)
m_values <- 6:8
l_values <- 0.9

# loading pars with calculate r
om_object <- om_object |> load_pars(list(
    # parametric sampling
    's' = distribution(pars = s_pars, density = "logitnormal", name = "adult survivorship"),
    'b' = distribution(pars = b_pars, density = "beta",        name = "female fecundity"),
    # non-parametric sampling
    'm' = distribution(values = m_values, name = "age at maturity"),
    'l' = distribution(values = l_values, name = "age zero survivorship multiplier")
))

# assign rmax
om_object <- om_object |> load_rmax()

# assign selectivity
om_object <- om_object |> update_pars(list(
    # non-parametric sampling
    'v' = distribution(value = m_values, name = "age at selectivity"),
	'o' = distribution(value = m_values, name = "age at observation")
))

# load default uncertainty
om_object <- om_object |> load_cvs(list(
    'survivorship' = 0.05,
    'birth'        = 0.01,
    'numbers'      = 0.00,
    'capture'      = 0.00)
)
om_object <- om_object |> load_quantiles(list(
    'numbers'  = 0.00)
)

# estimate reference points
om_object <- om_object |> shape(depletion = 0.6) |> rp()

# save
om_hdo <- om_object

use_data(om_hdo, overwrite = TRUE)
