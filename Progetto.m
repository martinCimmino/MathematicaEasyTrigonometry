(* ::Package:: *)

(* :Title: Progetto.m *)


(* :Context: Introduzione alla Trigonometria *)

(* :Author: Passaretti Simone
			Cappella Matteo
			Cimmino Martin *)

(* :Summary:
   Package contenente un'esemplificazione delle basi della trigonometria
 *)

(* :Mathematica Version: 10.0*)


(* :Sources:   biblio *)


BeginPackage["Progetto`"]

DrawSinCosDyn::usage = "DrawSinCosDyn[] effettua l'animazione del cerchio unitario
con all'interno la rappresentzione di sin cos tan di alpha"

DrawSinCosStat::usage = "DrawSinCosStat[] effettua una rappresentazione statica 
del cerchio unitario con all'interno la rappresentzione di sin cos tan di alpha"

DrawSinStat::usage = "DrawSinStat[] effettua una rappresentazione statica 
del cerchio unitario mettendo in evidenza la rappresentzione del seno dell'angolo alpha"

DrawCosStat::usage = "DrawCosStat[] effettua una rappresentazione statica 
del cerchio unitario mettendo in evidenza la rappresentzione del coseno dell'angolo alpha"

DrawTanStat::usage = "DrawTanStat[] effettua una rappresentazione statica 
del cerchio unitario mettendo in evidenza la rappresentzione del seno dell'angolo alpha"

DrawPeriodicSin::usage = "DrawPeriodicSin[] disegna un grafico che mostra 
la periodicit\[AGrave] della funzione sin in relazione al suo valore all'interno 
della circonferenza unitaria"

DrawPeriodicCos::usage = "DrawPeriodicCos[] disegna un grafico che mostra 
la periodicit\[AGrave] della funzione cos in relazione al suo valore all'interno 
della circonferenza unitaria";

DrawPeriodicTan::usage = "DrawPeriodicTan[] disegna un grafico che mostra 
la periodicit\[AGrave] della funzione tan in relazione al suo valore all'interno 
della circonferenza unitaria"

Riepilogo::usage = "DrawAll[] mostra un men\[UGrave] attraverso il quale si pu\[OGrave] accedere ai vari 
grafici del seno, coseno e tangente e un riepilogo delle tre che mettono in relazione i loro valori con l'andamento
delle loro rispettive funzioni"

LoadImage::usage = "LoadImage[name] carica un'immagine all'interno del file .nb"

Begin["`Private`"]

(*angolo \[EGrave] una variabile sfruttata per effettuare una rappresentazine statica delle funzioni*)
angolo = 50 Degree;

DrawSinCosStat[]:= Show[Graphics[
			    {  
					(*Definizione delle lettere nella figura dei due triangoli*)
				  Text["O",{-0.05,-0.05}],  
				  Text["A",{Cos[angolo],-0.05}],
				  Text["B",{Cos[angolo],Sin[angolo]+0.05}],
				  Text["C",{-0.05,Sin[angolo]}],
			      Text["D",{1.05,0.05}],
				  Text["E",{1.05,Tan[angolo]}],
					(*definizione del cerchio trigonometrico di raggio 1*)
				  Circle[], 
					(*definizione delle due linee tratteggiate che compiaono nella figura *)
				  {
				   Dashed,
				   Line[{{Cos[angolo], Sin[angolo]},{1,Tan[angolo]}}],
				   Line[{{Cos[angolo], Sin[angolo]},{0,Sin[angolo]}}]
				  },
					(*definizione del disco che rappresenta l'ampiezza del'angolo*)
				  {Yellow,Disk[{0,0},.2,{0,angolo}]},
					(*definizione delle linee che rappresentano del raggio, sin, cos, tan*)
				  {
				     Thick,
				     Line[{{0,0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Green, Green}], 
				     Line[{{0,0}, {Cos[angolo], 0}},VertexColors->{Red, Red}],
				     Line[{{Cos[angolo], 0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Blue, Blue}], 
				     Line[{{1,0}, {1,Tan[angolo]}},VertexColors->{Purple, Purple}]
				  }
			      }, 
					(*visualizzazione degli assi*)
				  Axes -> True]
			   ]

DrawSinStat[]:= Show[Graphics[
			    {  
					(*Definizione delle lettere nella figura dei due triangoli*)
				  Text["O",{-0.05,-0.05}],  
				  Text["A",{Cos[angolo],-0.05}],
				  Text["B",{Cos[angolo],Sin[angolo]+0.05}],
				  Text["C",{-0.05,Sin[angolo]}],
					(*definizione del cerchio trigonometrico di raggio 1*)
				  Circle[], 
					(*definizione della linea tratteggiata che compare nella figura *)
				  {
				   Dashed,
				   Line[{{Cos[angolo], Sin[angolo]},{0,Sin[angolo]}}]
				  },
					(*definizione del disco che rappresenta l'ampiezza del'angolo*)
				  {Yellow,Disk[{0,0},.2,{0,angolo}]},
					(*definizione delle linee che rappresentano del raggio, sin, cos*)
				  {
				     Thick,
				     Line[{{0,0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Black, Black}], 
				     Line[{{0,0}, {Cos[angolo], 0}},VertexColors->{Black, Black}],
				     Line[{{Cos[angolo], 0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Blue, Blue}]
				  }
			      }, 
					(*visualizzazione degli assi*)
				  Axes -> True]
			   ]


DrawCosStat[]:= Show[Graphics[
			    {  
					(*Definizione delle lettere nella figura dei due triangoli*)
				  Text["O",{-0.05,-0.05}],  
				  Text["A",{Cos[angolo],-0.05}],
				  Text["B",{Cos[angolo],Sin[angolo]+0.05}],
				  Text["C",{-0.05,Sin[angolo]}],
					(*definizione del cerchio trigonometrico di raggio 1*)
				  Circle[],
					(*definizione della linea tratteggiata che compare nella figura *) 
				  {
				   Dashed,
				   Line[{{Cos[angolo], Sin[angolo]},{0,Sin[angolo]}}]
				  },
					(*definizione del disco che rappresenta l'ampiezza del'angolo*)
				  {Yellow,Disk[{0,0},.2,{0,angolo}]},
					(*definizione delle linee che rappresentano del raggio, sin, cos*)
				  {
				     Thick,
				     Line[{{0,0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Black, Black}], 
				     Line[{{0,0}, {Cos[angolo], 0}},VertexColors->{Red, Red}],
				     Line[{{Cos[angolo], 0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Black, Black}]
				  }
			      }, 
					(*visualizzazione degli assi*)
				  Axes -> True]
			   ]

DrawTanStat[]:= Show[Graphics[
			    {  
				(*Definizione delle lettere nella figura dei due triangoli*)
				  Text["O",{-0.05,-0.05}],  
				  Text["A",{Cos[angolo],-0.05}],
				  Text["B",{Cos[angolo],Sin[angolo]+0.05}],
				  Text["C",{-0.05,Sin[angolo]}],
			      Text["D",{1.05,0.05}],
				  Text["E",{1.05,Tan[angolo]}],
				  Circle[], 
					(*definizione delle linee tratteggiate che compaiono nella figura *) 
				  {
				   Dashed,
				   Line[{{Cos[angolo], Sin[angolo]},{1,Tan[angolo]}}],
				   Line[{{Cos[angolo], Sin[angolo]},{0,Sin[angolo]}}]
				  },
					(*definizione del disco che rappresenta l'ampiezza del'angolo*)
				  {Yellow,Disk[{0,0},.2,{0,angolo}]},
					(*definizione delle linee che rappresentano del raggio, sin, cos e tan*)
				  {
				     Thick,
				     Line[{{0,0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Black, Black}], 
				     Line[{{0,0}, {Cos[angolo], 0}},VertexColors->{Black, Black}],
				     Line[{{Cos[angolo], 0}, {Cos[angolo], Sin[angolo]}},VertexColors->{Black, Black}], 
				     Line[{{1,0}, {1,Tan[angolo]}},VertexColors->{Purple, Purple}]
				  }
			      }, 
					(*visualizzazione degli assi*)
				  Axes -> True]
			   ]



DrawSinCosDyn[]:= Manipulate[Show[Graphics[
			    { 
					(*Definizione del label degli assi *)
				   Text["0",{1.1,0}],  
                   Text["\[Pi]/2",{0,1.1}],
				   Text["\[Pi]",{-1.1,0}],   
				   Text["3/2\[Pi]",{0,-1.1}],    
					(*Definizione del cerchio trigonometrico di raggio 1*)   
				  Circle[], 
					(*Definizione della linea tratteggiata presente nella figura*)
				  {Dashed,Line[{{Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]},{1,Tan[\[Alpha] Degree]}}]},
					(*definizione del disco che rappresenta l'ampiezza del'angolo*)
				  {Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
					(*definizione delle linee che rappresentano del raggio, sin, cos e tan*)
				  {
				     Thick,
				     Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
				     Line[{{0,0}, {Cos[\[Alpha] Degree], 0}},VertexColors->{Red, Red}],
				     Line[{{Cos[\[Alpha] Degree], 0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Blue, Blue}], 
				     Line[{{1,0}, {1,Tan[\[Alpha] Degree]}},VertexColors->{Purple, Purple}]
				  }
			      }, 
					(*visualizzazione degli assi*)
				     Axes -> True,
					(*Definizione di un frame intorno al grafico per mantenere la sua ratio*)
					Frame->True,PlotRange->2.5,PlotRangeClipping->False],
					(*Definizione della legenda presente nella figura*)
				  Epilog->Inset[Framed[
						Column[
								{LineLegend[{Blue,Red,Purple},{"Sin", "Cos", "Tan"}]}
							]
							],Scaled[{.85,.75}]]
			   ], 
				(*Intervallo dei valori che l'angolo \[Alpha] pu\[OGrave] assumere*)
				{{\[Alpha], 0 ,"\[Alpha]"},0,360}]

DrawPeriodicSin[]:=Manipulate[
			Row[{
						Graphics[{
							(*Definizione del label degli assi *)
							Text[Style["0",Large],{1.25,0}],  
                                       Text[Style["\[Pi]/2",Large],{0,1.25}],
				             Text[Style["\[Pi]",Large],{-1.25,0}],   
				             Text[Style["3/2\[Pi]",Large],{0,-1.25}], 
							(*Definizione del cerchio trigonometrico di raggio 1*)  
							Circle[],
							(*definizione del disco che rappresenta l'ampiezza del'angolo*)	
							{Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
							{
								(*definizione delle linee che rappresentano il raggio e  il sin*)
								Thick,
								Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
								Line[{{Cos[\[Alpha] Degree], 0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Blue, Blue}]
							}
						},
							(*visualizzazione degli assi*)
							Axes->True, 
							(*Dimensione assegnata al grafico che verr\[AGrave] visualizzata*)
							ImageSize->Medium],
						(*Disegno del grafico della funzione seno nell'intervallo [0,4Pi] *)					
						Plot[Sin[x Degree],{x,0,720},
						(*Definizione del punto che si muove dinamicamente lungo la funzione*)
						Epilog->{PointSize[Medium],Dynamic@Point[{\[Alpha],Sin[\[Alpha] Degree]}]},
						(*Dimensione assegnata al Plot*)
						ImageSize->Medium, 
						(*Valori visualizzati rispettivamente sull'ascissa e sull'ordinata*)
						Ticks->{{0,90,180,270,360 ,450,540,630, 720},{-1,1}}]
						}],
						(*Intervallo dei valori che l'angolo \[Alpha] pu\[OGrave] assumere*)
						{{\[Alpha], 0 ,"\[Alpha]"},0, 720}]

DrawPeriodicCos[]:= Manipulate[Row[{
						Graphics[{
							(*Definizione del label degli assi *)
							Text[Style["0",Large],{1.25,0}],  
                             Text[Style["\[Pi]/2",Large],{0,1.25}],
				             Text[Style["\[Pi]",Large],{-1.25,0}],   
				             Text[Style["3/2\[Pi]",Large],{0,-1.25}], 
							(*Definizione del cerchio trigonometrico di raggio 1*)  
							Circle[],
							(*definizione del disco che rappresenta l'ampiezza del'angolo*)
							{Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
							{
							(*definizione delle linee che rappresentano il raggio e  il cos*)
								Thick,
								Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
								Line[{{0,0}, {Cos[\[Alpha] Degree], 0}},VertexColors->{Red, Red}]
							}
						},
							(*visualizzazione degli assi*)
							Axes->True, 
							(*Dimensione assegnata al grafico che verr\[AGrave] visualizzata*)
							ImageSize->Medium],
						(*Disegno del grafico della funzione coseno nell'intervallo [0,4Pi] *)										
						Plot[Cos[x Degree],{x,0,720},
						(*Definizione del punto che si muove dinamicamente lungo la funzione*)
						Epilog->{PointSize[Medium],Dynamic@Point[{\[Alpha],Cos[\[Alpha] Degree]}]},
						(*Dimensione assegnata al Plot*)
						ImageSize->Medium,
						(*Valori visualizzati rispettivamente sull'ascissa e sull'ordinata*)
						 Ticks->{{0,90,180,270,360 ,450,540,630, 720},{-1,1}}]
						}],
						(*Intervallo dei valori che l'angolo \[Alpha] pu\[OGrave] assumere*)
						{{\[Alpha], 0 ,"\[Alpha]"},0, 720}]

DrawPeriodicTan[]:= Manipulate[Row[{
						Graphics[{
							(*Definizione del label degli assi *)
							Text[Style["0",Large],{1.25,0}],  
                             Text[Style["\[Pi]/2",Large],{0,1.25}],
				             Text[Style["\[Pi]",Large],{-1.25,0}],   
				             Text[Style["3/2\[Pi]",Large],{0,-1.25}],  
							(*Definizione del cerchio trigonometrico di raggio 1*)  
							Circle[],
							(*definizione del disco che rappresenta l'ampiezza del'angolo*)
							{Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
							{
							(*definizione delle linee che rappresentano il raggio e  il cos*)
								Thick,
								Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
								Line[{{1,0}, {1,Tan[\[Alpha] Degree]}},VertexColors->{Purple, Purple}]
							},
							(*Definizione della linea tratteggiata presente nella figura*)
							 {Dashed,Line[{{Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]},{1,Tan[\[Alpha] Degree]}}]}
						},
						(*visualizzazione degli assi*)
						Axes->True, 
						(*Dimensione assegnata al grafico che verr\[AGrave] visualizzata*)
						ImageSize->Medium, 
						(*Definizione di un frame intorno al grafico per mantenere la sua ratio*)
						Frame->True,PlotRange->2.5,PlotRangeClipping->False],					
						(*Disegno del grafico della funzione tangente nell'intervallo [0,4Pi] *)						
						Plot[Tan[x Degree],{x,0,720},
						(*Definizione del punto che si muove dinamicamente lungo la funzione*)
						Epilog->{PointSize[Medium],Dynamic@Point[{\[Alpha],Tan[\[Alpha] Degree]}]},
						(*Dimensione assegnata al Plot*)
						ImageSize->Medium, 
						(*Valori visualizzati rispettivamente sull'ascissa e sull'ordinata*)
						 Ticks->{{0,90,180,270,360 ,450,540,630, 720},{-1,1}}]
						}],
						(*Intervallo dei valori che l'angolo \[Alpha] pu\[OGrave] assumere*)
						{{\[Alpha], 0 ,"\[Alpha]"},0,720}]



opTable[o_,\[Alpha]_]/; o==Sin :=Row[{
					Graphics[{
                         (*Definizione del label degli assi *)
							Text[Style["0",Large],{1.25,0}],  
                             Text[Style["\[Pi]/2",Large],{0,1.25}],
				             Text[Style["\[Pi]",Large],{-1.25,0}],   
				             Text[Style["3/2\[Pi]",Large],{0,-1.25}],  
                            (*Definizione del cerchio trigonometrico di raggio 1*)  
							Circle[],
                		   (*definizione del disco che rappresenta l'ampiezza del'angolo*)						    
							{Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
							{
                         (*definizione delle linee che rappresentano il raggio e  il cos*)
								Thick,
								Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
				                Line[{{Cos[\[Alpha] Degree], 0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Blue, Blue}]              
							}
						},
                         (*visualizzazione degli assi*)
                         Axes->True,
                         (*Definizione di un frame intorno al grafico per mantenere la sua ratio*)
                         Frame->True,PlotRange->2.5,PlotRangeClipping->False, 
                         (*Dimensione assegnata al grafico che verr\[AGrave] visualizzata*)
                         ImageSize->Large, 
                         (*Definizione del punto che si muove dinamicamente lungo la funzione*)
                         Epilog->Inset[Framed[
						Column[
						    {LineLegend[{Blue,Red,Purple},{"Sin", "Cos", "Tan"}]}
						]
						],Scaled[{.85,.75}]]
					    ],	
                        (*Disegno del grafico della funzione nell'intervallo [0,4Pi] *)					
						Plot[o[x Degree],{x,0,720},
                         (*Definizione del punto che si muove dinamicamente lungo la funzione*)
						Epilog->{PointSize[Medium],Dynamic@Point[{\[Alpha],o[\[Alpha] Degree]}]},
                         (*Dimensione assegnata al Plot*)
                         ImageSize->Large, 
                         (*Valori visualizzati rispettivamente sull'ascissa e sull'ordinata*)
                         Ticks->{{0,90,180,270,360 ,450,540,630, 720},{-1,1}}]
						}]

opTable[o_,\[Alpha]_]/; o==Cos :=Row[{
					Graphics[{
                             (*Definizione del label degli assi *)
							Text[Style["0",Large],{1.25,0}],  
                             Text[Style["\[Pi]/2",Large],{0,1.25}],
				             Text[Style["\[Pi]",Large],{-1.25,0}],   
				             Text[Style["3/2\[Pi]",Large],{0,-1.25}],  
                             (*Definizione del cerchio trigonometrico di raggio 1*)  
							Circle[],
				            (*definizione del disco che rappresenta l'ampiezza del'angolo*)						    
							{Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
							{
                             (*definizione delle linee che rappresentano il raggio e  il cos*)
								Thick,
								Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
				                Line[{{0,0}, {Cos[\[Alpha] Degree],0}},VertexColors->{Red, Red}]            
							}
						},
                        (*visualizzazione degli assi*)
                        Axes->True,
                        (*Definizione di un frame intorno al grafico per mantenere la sua ratio*)
                        Frame->True,PlotRange->2.5,PlotRangeClipping->False, 
                        (*Dimensione assegnata al grafico che verr\[AGrave] visualizzata*)
                        ImageSize->Large, 
                        (*Definizione del punto che si muove dinamicamente lungo la funzione*)
                        Epilog->Inset[Framed[
						Column[
						    {LineLegend[{Blue,Red,Purple},{"Sin", "Cos", "Tan"}]}
						]
						],Scaled[{.85,.75}]]
					    ],	
                        (*Disegno del grafico della funzione nell'intervallo [0,4Pi] *)					
						Plot[o[x Degree],{x,0,720},
						Epilog->{PointSize[Medium],Dynamic@Point[{\[Alpha],o[\[Alpha] Degree]}]},
                         (*Dimensione assegnata al Plot*)
                          ImageSize->Large,
                          (*Valori visualizzati rispettivamente sull'ascissa e sull'ordinata*)
                          Ticks->{{0,90,180,270,360 ,450,540,630, 720},{-1,1}}]
						}]

opTable[o_,\[Alpha]_]/; o==Tan :=Row[{
					Graphics[{
                            (*Definizione del label degli assi *)
							Text[Style["0",Large],{1.25,0}],  
                             Text[Style["\[Pi]/2",Large],{0,1.25}],
				             Text[Style["\[Pi]",Large],{-1.25,0}],   
				             Text[Style["3/2\[Pi]",Large],{0,-1.25}],  
                            (*Definizione del cerchio trigonometrico di raggio 1*)  
							Circle[],
           				(*definizione del disco che rappresenta l'ampiezza del'angolo*)						    
							{Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
                            (*Definizione della linea tratteggiata presente nella figura*)
                             {Dashed,Line[{{Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]},{1,Tan[\[Alpha] Degree]}}]},
							{
                            (*definizione delle linee che rappresentano il raggio e  il cos*)
								Thick,
								Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
				                Line[{{1,0}, {1,Tan[\[Alpha] Degree]}},VertexColors->{Purple, Purple}]            
							}
						},
                        (*visualizzazione degli assi*)
                         Axes->True,
                         (*Definizione di un frame intorno al grafico per mantenere la sua ratio*)
                         Frame->True,PlotRange->2.5, PlotRangeClipping->False,
                         (*Dimensione assegnata al grafico che verr\[AGrave] visualizzata*)
                         ImageSize->Large, 
                         (*Definizione del punto che si muove dinamicamente lungo la funzione*)
                         Epilog->Inset[Framed[
						Column[
						    {LineLegend[{Blue,Red,Purple},{"Sin", "Cos", "Tan"}]}
						]
						],Scaled[{.85,.75}]]
					    ],	
                         (*Disegno del grafico della funzione nell'intervallo [0,4Pi] *)					
						Plot[o[x Degree],{x,0,720},
						Epilog->{PointSize[Medium],Dynamic@Point[{\[Alpha],o[\[Alpha] Degree]}]},
                         (*Dimensione assegnata al Plot*)
                        ImageSize->Large,
                          (*Valori visualizzati rispettivamente sull'ascissa e sull'ordinata*)
                          Ticks->{{0,90,180,270,360 ,450,540,630, 720},{-1,1}}]
						}]

opTable[o_,\[Alpha]_]/; o=="All":=Row[{
					Graphics[{
                            (*Definizione del label degli assi *)
							Text[Style["0",Large],{1.25,0}],  
                             Text[Style["\[Pi]/2",Large],{0,1.25}],
				             Text[Style["\[Pi]",Large],{-1.25,0}],   
				             Text[Style["3/2\[Pi]",Large],{0,-1.25}],  
                      	  (*Definizione del cerchio trigonometrico di raggio 1*)  
							Circle[],
             				(*definizione del disco che rappresenta l'ampiezza del'angolo*)						    
							{Yellow,Disk[{0,0},.2,{0,\[Alpha] Degree}]},
                             (*Definizione della linea tratteggiata presente nella figura*)
                             {Dashed,Line[{{Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]},{1,Tan[\[Alpha] Degree]}}]},
							{
                             (*definizione delle linee che rappresentano il raggio e  il cos*)
								Thick,
								Line[{{0,0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Green, Green}], 
				                Line[{{1,0}, {1,Tan[\[Alpha] Degree]}},VertexColors->{Purple, Purple}],
								Line[{{0,0}, {Cos[\[Alpha] Degree],0}},VertexColors->{Red, Red}], 
								Line[{{Cos[\[Alpha] Degree], 0}, {Cos[\[Alpha] Degree], Sin[\[Alpha] Degree]}},VertexColors->{Blue, Blue}]          
							}
						},
                         (*visualizzazione degli assi*)
                         Axes->True,
                         (*Definizione di un frame intorno al grafico per mantenere la sua ratio*)
                         Frame->True,PlotRange->2.5,PlotRangeClipping->False, 
                         (*Dimensione assegnata al grafico che verr\[AGrave] visualizzata*)
                         ImageSize->Large, 
                         (*Definizione della legenda*)
                         Epilog->Inset[Framed[
						Column[
						    {LineLegend[{Blue,Red,Purple},{"Sin", "Cos", "Tan"}]}
						]
						],Scaled[{.85,.75}]]
					    ]						
						}]


(*Chiamata della funzione definita in tre modi differenti sulla base del parametreo scelto*)
Riepilogo[]:=Manipulate[opTable[funzione,\[Alpha]],{{\[Alpha], 0 ,"\[Alpha]"},0,720},{{funzione, Sin ,"Funzione"},{Sin,Cos,Tan,"All"}}]

LoadImage[name_]:=Import[DirectoryName[ToFileName["FileName"/.NotebookInformation[SelectedNotebook[]]]]<>name]


End[ ]

EndPackage[ ]
