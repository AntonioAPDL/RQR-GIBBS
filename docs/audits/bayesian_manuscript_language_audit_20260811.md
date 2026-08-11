# Bayesian Manuscript Language Audit

Date: 2026-08-11

## Scope

This audit reviewed the complete prose of `main.tex` and
`rqr-gibbs-supplement.tex`, including abstracts, theorem commentary,
algorithms, tables, figure captions, evaluation statements, and limitations.
The objective was to make the exposition natural to a Bayesian-statistics
reader without turning the RQR loss update into an ordinary response model.

The revision is terminological and editorial. It does not change the loss,
population interval functional, prior specification, latent-variable
identity, full conditional distributions, Gibbs order, empirical results, or
scope of any validation claim.

## Terminology contract

| Statistical object | Preferred manuscript language | Language avoided after definition |
|---|---|---|
| Prior combined with the fixed-rate loss update | generalized posterior | posterior-like update; declared target |
| Unnormalized density when normalization is irrelevant | generalized-posterior kernel or factor | exponentiated loss contribution |
| Latent-scale representation | augmented generalized posterior | response model; likelihood augmentation |
| One Gibbs update | full conditional distribution | Gaussian block; conditional draw |
| Ordered endpoint summaries | generalized-posterior functionals or credible bands | posterior predictive response intervals |
| Future endpoint propagation | future root or endpoint functionals | response prediction or response simulation |
| Population interval placement | target or estimand | posterior, unless uncertainty is intended |

The word `target` remains appropriate for a population estimand, such as the
ordinary-RQR or equal-tailed interval functional. It is not used as a vague
substitute for the generalized posterior when posterior computation is meant.

## Principal revisions

1. The abstract and introduction now define the generalized posterior in
   standard prior-times-loss-update language.
2. The pseudo-asymmetric-Laplace identity is described as a latent-variable
   augmentation of the generalized posterior. It is explicitly not a sampling
   density for the response.
3. Static coefficient and dynamic state updates are called Gaussian full
   conditionals. Latent-scale and learned-scale updates are named by their GIG
   and Gamma full conditional distributions.
4. The root-blocked DLM discussion distinguishes the jointly quartic augmented
   generalized posterior from the root-specific Gaussian full conditionals.
5. Learned inverse-loss scale language now consistently identifies a
   hierarchical generalized posterior and retains the fixed-rate and nonzero-
   tilt limitations.
6. Figure captions identify uncertainty ribbons and error intervals as
   generalized-posterior summaries of endpoint functionals, not posterior
   predictive response intervals.
7. Evaluation and discussion language distinguishes generalized-posterior
   validation from frequentist coverage calibration and predictive-response
   validation.

## Automated guard

`application/scripts/63_validate_manuscript_bayesian_language.R` checks both
manuscripts for deprecated phrases, required Bayesian terminology, and a small
set of scientifically unsafe response-prediction claims. It is available as:

```bash
make test-manuscript-language
```

This guard is deliberately narrow. It protects the central interpretation and
vocabulary but does not replace substantive statistical or editorial review.

## Validation record

| Check | Result |
|---|---|
| `make test-manuscript-language` | passed; 1,316 main-text lines and 1,808 supplement lines checked |
| `make test-theory-figures` | passed; deterministic regeneration and oracle checks |
| `make test-theory-tables` | passed; deterministic CF population-table checks |
| `make pdf` | passed; 23-page manuscript |
| `make supplement` | passed; 28-page supplement |
| Final LaTeX log scan | no unresolved citations or references, overfull boxes, underfull boxes, or multiply defined labels |
| Visual inspection | passed for the abstract and introduction; static generalized posterior and augmentation; learned scale; dynamic state section; and corresponding supplement pages |
| `make arxiv-source` | passed; ZIP integrity check reported no errors and the package contained no review- or assistant-named files |

Generated PDFs and the arXiv archive remain ignored build artifacts. Figure
binaries regenerated during validation were restored to their tracked bytes;
the only figure-source revision is the Bayesian wording inside the DLM blocked-
state schematic.
