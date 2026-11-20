# MTI with a Proper Prior on Common Loss Scale

This experimental extension estimates a common loss scale for fixed fitted
content q and fixed tilt delta. The response-distribution DP used for interval
content remains a separate model. Current manuscript results retain fixed
scale; no new empirical superiority or tolerance guarantee is asserted.

Let e_i=(y_i-a)(y_i-b), rho_q(e)=e[q-1(e<0)], and
T=q delta sum_i(a+b-2y_i). Put L=sum_i rho_q(e_i)-T. Independent Gaussian
root priors have variance tau^2. Define eta=log(omega) and sigma=1/omega.
On independently declared finite bounds l<0<u, the prior for eta is uniform.
The joint generalized posterior, relative to da db d eta, is

    pi(a,b,eta | D,q,delta) proportional to
      exp{n eta-exp(eta)L-(a^2+b^2)/(2 tau^2)} 1(l<=eta<=u).

The factor omega^n adopts the existing normalized pseudo-residual convention;
it does not turn the product residual into a response likelihood. Omitting
this factor defines another joint target. Terms in L constant in a,b must
be retained because they depend on the unknown scale after multiplication
by omega. A mode in eta is not the reciprocal of a mode in sigma.

## Propriety

For a finite dataset, write M=exp(u) and A=nq abs(delta). Since rho_q>=0,

    exp{-omega L} <= exp{MA(|a|+|b|)+2Mq abs(delta) sum_i |y_i|}.

The factor exp(n eta) is bounded by exp(nu), the eta prior has finite support,
and Gaussian priors have all linear exponential moments. Integrating this
bound gives a finite positive normalizing constant. This proof concerns
fixed finite candidate delta and proper Gaussian roots. It establishes
existence, not identifiability, bound insensitivity or tolerance attainment.

An unrestricted prior proportional to 1/sigma can fail: a negative loss on
an open set of roots yields exponential growth as omega tends to infinity.
An ordinary Gamma prior on omega likewise fails if its rate plus L is
negative on such a set. The example y=(-0.1,0.1), q=0.95, delta=0.5,
(a,b)=(0,20) has L=-16.991. A positive conditional rate at one fitted mode
does not supply a global propriety argument. A bounded prior is proper and
range-dependent; it should not be called an objective noninformative prior.

## ECM and ECME

Let xi=(1-2q)/[q(1-q)] and phi=2/[q(1-q)]. The augmentation uses

    e_i | v_i,omega ~ Normal(xi v_i,phi v_i/omega),
    v_i | omega ~ Exponential(rate=omega),

multiplied by exp(omega T). Its rate power is 3n/2. At current nonzero e_i,

    E(1/v_i)=1/[q(1-q)|e_i|],
    E(v_i)=q(1-q)(|e_i|+2/omega).

The E-step evaluates both moments at the previous roots and rate. Holding
these fixed, the two Gaussian root conditional modes are unchanged except
for the current omega. The root prior precision remains 1/tau^2, independent
of omega. After updating roots, put

    H=sum_i[e_i^2 E(1/v_i)/(2phi)-xi e_i/phi
            +(xi^2/(2phi)+1)E(v_i)]-T.

The conditional eta maximization is log[(3n/2)/H], restricted to [l,u],
when H>0, and u when H<=0. ECME instead maximizes the observed joint
density: eta=log(n/L) restricted to [l,u] when L>0, and u when L<=0.
The implementation records the version and uses the full observed
objective J=exp(eta)L+(a^2+b^2)/(2tau^2)-n eta for safeguards.

At zero product residuals the inverse latent moment is infinite. The
declared numerical floor is an approximation. Exact observed one-root
conditional minimizations and the observed eta optimum polish the final
solution, with checks for possible objective improvement and endpoint
displacement. These establish coordinatewise accuracy, not global optimality.
Independent small-case profile references and multiple starts assess that
remaining uncertainty.

## Interpretation and Deferred Sampling

Rate learning changes relative loss/prior strength and may select a prior
bound. Widening support is not automatically less influential. The experiment
reports bound occupation and root/width sensitivity before advancing.
The primary range [0.25,4] allows a factor two change in effective Gaussian
root-mode regularization SD around SD2; [0.5,2] and [0.0625,16] are stated
sensitivity ranges. No bound uses an analyzed sample or a validation width.

Conditional on roots, omega has density proportional to
omega^(n-1) exp(-omega L) on [exp(l),exp(u)]. It is a positive-rate
truncated Gamma only when L>0; a bounded scalar density is still proper
when L<=0. No sampler for this nonzero-tilt extension is enabled here.
Gibbs would require independent distributional checks and its own tolerance
assessment. ECM returns a joint mode and does not integrate scale uncertainty.

The general rationale for loss-based updating is given by
[Bissiri, Holmes and Walker (2016)](https://arxiv.org/abs/1306.6430).
The normal-exponential representation follows
[Kozumi and Kobayashi (2011)](https://www.b.kobe-u.ac.jp/papers_files/2009_02.pdf),
whose response-regression model differs from this generalized interval loss.
The bounded-prior argument and joint log-scale ECM derivation above concern
this MTI formulation specifically.
