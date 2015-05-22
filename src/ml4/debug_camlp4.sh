#!/bin/bash
camlp4r pa_o.cmo pr_o.cmo pa_macro.cmo ./ml4lib.cma -QD a.err -verbose  $1
