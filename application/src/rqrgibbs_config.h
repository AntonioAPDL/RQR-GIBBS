#ifndef RQRGIBBS_CONFIG_H
#define RQRGIBBS_CONFIG_H

// Some institutional R toolchains define _GLIBCXX_ASSERTIONS globally. Its
// replacement assertion writes with printf() and calls abort(), which violates
// R's compiled-code policy even when the package never invokes that path.
// Native rqrgibbs dimension and SPD checks raise R errors explicitly.
#ifdef _GLIBCXX_ASSERTIONS
#undef _GLIBCXX_ASSERTIONS
#endif

#endif
