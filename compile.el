(require 'org)
(org-mode)
(org-link-set-parameters "chck"
			 :export (lambda (path desc format)
				   (format "<chck value=\"%s\"></chck>" path)))
(org-html-export-as-html nil nil nil t)
(princ (buffer-string))

