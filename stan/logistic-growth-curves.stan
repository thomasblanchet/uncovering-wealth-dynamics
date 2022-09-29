// -------------------------------------------------------------------------- //
// Stan program used to fit logistic growth curves to model the log CCDF
// (and therefore get its derivative wrt. time)
// -------------------------------------------------------------------------- //

functions {
    // Logistic growth curve, parametrized with initial value (y0), final
    // value (yinf) and rate parameter (rate)
    vector logis_curve(vector t, real y0, real yinf, real rate) {
        return(yinf ./ (1 + (yinf/y0 - 1)*exp(-rate*t)));
    }
}

data {
    int<lower=0> n;
    // Time index
    vector[n] t;
    // Value of the curve
    vector[n] y;
}

transformed data {
    real yfirst = y[1];
    real zfirst = yfirst - log1m_exp(yfirst);
}

parameters {
    // Parameters of the logistic curve
    real z0;
    real zinf;
    real<lower=0> half_life;

    // Parameters for the random noise around the curve
    real<lower=0> sigma;
}

transformed parameters {
    real<upper=0> y0   = -log1p_exp(-z0);
    real<upper=0> yinf = -log1p_exp(-zinf);
    real<lower=0> rate = log1p(yinf/y0)/half_life;
}

model {
    // Priors for the noise
    sigma ~ normal(0, 1) T[0, ];

    // Priors for the logistic curve

    // y0 close to yfirst value (use a tight prior here since we this is
    // almost a direct estimation)
    z0 ~ normal(zfirst, 0.5);
    // yinf close to twice the distance between ylast and yfirst (i.e.
    // we're about at the half-life), but use a weakly informative prior since
    // this is just an order-of-magnitude estimate
    zinf ~ normal(z0, 5);
    // Half-life of about 10 years, but with a fairly wide spread since
    // this an order-of-magnitude estimate
    half_life ~ normal(10, 5) T[0, ];

    // Minimize sum of squares
    y ~ normal(logis_curve(t, y0, yinf, rate), sigma);
}

