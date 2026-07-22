# First Jerez Codex Prompt

Use this prompt when opening the first Codex chat on Jerez.

```text
You are working on jerez.be.ucsc.edu in the standalone RQR-GIBBS project.

Primary repo:
/data/muscat_data/jaguir26/local/src/RQR-GIBBS

Reference implementation repo:
/data/muscat_data/jaguir26/local/src/exdqlm__wt__qdesn_0p4p0_integration
branch: feature/rqr-desn-readout-20260716
expected commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN style/reference repo:
/data/muscat_data/jaguir26/local/src/Article-Q-DESN---Version-2

First, do a read-only verification. Confirm:
1. RQR-GIBBS is on main at 4866b5572b600b88ca389be41e5830c4ad1326c7 or newer.
2. exdqlm is on feature/rqr-desn-readout-20260716 at dffb71ee70b597d6a716ee74be1cbc99731cd453.
3. The three repos are clean or identify any dirty files exactly.
4. `make smoke`, `make pdf`, `make supplement`, and `make test-exdqlm-rqr` pass.
5. `literature/pdfs/` contains the local-only PDF set and `make literature-manifest` works.

Then prepare a careful plan, without launching heavy jobs yet, for the next
standalone RQR-GIBBS stage:
- promote or wrap the RQR implementation into a native standalone API;
- implement and test the RQR-DLM/FFBS path;
- design the matched standalone simulation comparing fixed-design RQR,
  RQR-DESN, RQR-DLM, quantile-derived intervals, and empirical baselines;
- keep all large outputs under ignored local directories;
- document every gate, exact commit, seed, and manifest.

Do not mutate the Q-DESN article repo from this chat. Treat it only as a style
reference unless explicitly instructed otherwise.
```

