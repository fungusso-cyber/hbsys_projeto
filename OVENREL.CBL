       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVENREL.
       AUTHOR. GERSON GUSSO.
       DATE-WRITTEN. DEZ-2019.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
		   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	        
       DATA DIVISION.
       FILE SECTION.
	   
       WORKING-STORAGE SECTION.
        
       77 KEYSTATUS						 PIC 9(004) SPECIAL-NAMES CRT STATUS.
	      88 ESCAPE-KEY                  VALUE 27.
	      88 LIMPA-KEY 					 VALUE 221.
              88 GERA-KEY                    VALUE 222.
       01 WS-CAMPOS-OK                   PIC X      VALUE SPACES.  	    
       01 WS-LIGACAO.
          05 WS-TIPO-ORD                 PIC X      VALUE SPACES.
          05 WS-CAMPO                    PIC X      VALUE SPACES. 
          05 WS-CODIGO                   PIC 9(007) VALUE ZEROS.
          05 WS-RAZAO                    PIC X(030) VALUE SPACES.		  
           
       SCREEN SECTION.
       COPY "TELVENREL.SCR".
       LINKAGE SECTION.
       01 LK-RETORNO-IMP.
          05 LK-TIPO-ORD                 PIC X.       
          05 LK-CAMPO                    PIC X.       
          05 LK-CODIGO                   PIC 9(007). 
          05 LK-RAZAO                    PIC X(030). 		  
          05 LK-RETORNO                  PIC X(030).
   	   	   
       PROCEDURE DIVISION USING LK-REOTORNO-IMP.
       1000-INICIO.
	                
           DISPLAY TELA.
	    
       2000-PROCESSA.
	   
           PERFORM WITH TEST AFTER UNTIL ESCAPE-KEY
				ACCEPT TELA ON EXCEPTION
					PERFORM 4000-CONTROLE-TELA
				END-ACCEPT
           END-PERFORM.
		   
       3000-FINALIZA.
		   
 	   GOBACK.
		   
       4000-CONTROLE-TELA.
	       
		   EVALUATE TRUE
				WHEN GERA-KEY
			             
					PERFORM 4001-VALIDA-CAMPOS
					PERFORM 4002-GERA-RELATORIO
					
				
				WHEN LIMPA-KEY
					PERFORM 4006-LIMPA-TELA
                  				  
		   	END-EVALUATE.
		   
       4001-VALIDA-CAMPOS.
	       
		   MOVE SPACES           TO WS-CAMPOS-OK
		   IF T-TIPO-ORD NOT EQUAL "A" AND "D"
		      DISPLAY MESSAGE BOX
			  "TIPO DEVE SER A OU D  !"
			  ACCEPT T-TIPO-ORD AT LINE 3, COL 35
			  MOVE "N"           TO WS-CAMPOS-OK
			  
		   ELSE
		       IF T-CAMPO NOT EQUAL "C" AND "R"
			       DISPLAY MESSAGE BOX
			       "CAMPO DEVE SER C OU R !"
				   ACCEPT T-CAMPO AT LINE 5, COL 35
				   MOVE "N"           TO WS-CAMPOS-OK
			   ELSE  
                   	IF (T-CAMPO NOT EQUAL SPACES) AND
 					   (T-CODIGO NOT EQUAL ZEROS OR T-RAZAO NOT EQUAL SPACES)
                       			DISPLAY MESSAGE BOX
			           "OPÇÕES DE CLASSIFICAÇÃO INVALIDAS !"
					   ACCEPT T-CAMPO AT LINE 5, COL 35
					   MOVE "N"        TO WS-CAMPOS-OK
                  	  ELSE					   
		                IF (T-CODIGO NOT EQUAL ZEROS OR T-RAZAO NOT EQUAL SPACES) AND
						    (T-CAMPO NOT EQUAL SPACES)
                          	 DISPLAY MESSAGE BOX
			                "OPÇÕES DE CLASSIFICAÇÃO INVALIDAS !"
						   ACCEPT T-LONGITUDE AT LINE 7, COL 35
						   MOVE "N"        TO WS-CAMPOS-OK
						END-IF
					END-IF	
             		  END-IF	   
	       END-IF.
		   
       4002-GERA-RELATORIO.
           IF WS-CAMPOS-OK EQUAL SPACES
		      MOVE T-TIPO-ORD              TO WS-TIPO-ORD
			  MOVE T-CAMPO                 TO WS-CAMPO
			  MOVE T-CODIGO                TO WS-CODIGO
			  MOVE T-RAZAO                 TO WS-RAZAO
	          CALL "VENREL" USING WS-LIGACAO
			  IF LK-RETORNO NOT EQUAL SPACES
			     DISPLAY MESSAGE BOX
			     "ERRO NA EMISSÃO DO RELATÓRIO!"
				 PERFORM 4006-LIMPA-TELA
			     ACCEPT T-TIPO-ORD AT LINE 3, COL 35 
		      ELSE
			     DISPLAY MESSAGE BOX
			     "RELATÓRIO GERADO COM SUCESSO !"
		      END-IF
		   END-IF
					 
      4006-LIMPA-TELA.
		   
	  DISPLAY TELA.
		   
