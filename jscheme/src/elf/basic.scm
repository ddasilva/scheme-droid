(use-module "elf/util.scm" 'import 'all)

(use-module "elf/iterate.scm" 'import 'all)  ;; this imports util.scm

(use-module "elf/apropos.scm" 'import 'all)  ;; this imports util and iterate

(use-module "elf/describe.scm" 'import       ;; this imports util,sort,iterate,eopl2/jscheme/genwrite
	    '(memoize-1 all-fields all-methods short-toString
			printingToString ppString ppToString
			describe describe-object class-cpl describe-class
			revert backtraceBody backtraceValueString whoCalls))

(use-module "elf/inspect.scm" 'import '(inspect)) ;; this imports iterate.scm
(use-module "elf/sort.scm" 'import 'all)
(use-module "elf/eopl2/jscheme/genwrite.scm" 'import 'all) ; pp pretty-print
