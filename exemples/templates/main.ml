
        open Core
        let render  firstname lastname email birthdate phonenumber formations experiences  =
          let ___elements = ref [] in
          let ___append e =
            ___elements := e :: !___elements
          in
        ___append {___|
\documentclass[10pt, a4paper, roman, french]{moderncv}
\moderncvstyle{classic}                             
\moderncvcolor{purple}                              
\usepackage[utf8]{inputenc}
\usepackage[light]{CormorantGaramond}
\usepackage[T1]{fontenc}
\usepackage[scale=0.75,a4paper]{geometry}
\usepackage{babel}
\usepackage{geometry}
\geometry{hmargin=2.5cm,vmargin=1.5cm}

%----------------------------------------------------------------------------------
%            informations personnelles
%----------------------------------------------------------------------------------
\firstname{ |___} ;___append (firstname) ;___append {___| }
\familyname{ |___} ;___append (lastname) ;___append {___| }
\mobile{|___} ;___append ( phonenumber) ;___append {___|}                          
\extrainfo{Né le |___} ;___append (birthdate) ;___append {___| }
\email{ |___} ;___append (email) ;___append {___| }                               
\begin{document}
	\makecvtitle
	\section{Formation}
	 |___} ; List.iter formations ~f:(fun (date_start, date_end, diploma, school) ->___append {___|
		\cventry{ |___} ;___append (date_start) ;___append {___| -- |___} ;___append (date_end) ;___append {___|}{|___} ;___append (diploma) ;___append {___|}{|___} ;___append (school) ;___append {___|}{}{}{}
	 |___} ;) ;___append {___|
	\section{Expérience}
		 |___} ; List.iter experiences ~f:(fun (date, title, company, location, description) ->___append {___|
			\cventry{ |___} ;___append (date) ;___append {___| }{ |___} ;___append (title) ;___append {___| }{ |___} ;___append (company) ;___append {___| }{ |___} ;___append (location) ;___append {___| }{}{ |___} ;___append (description) ;___append {___| }
	 |___} ;) ;___append {___|
\end{document}|___} ;
  String.concat (List.rev !___elements)
  