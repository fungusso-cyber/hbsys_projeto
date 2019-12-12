       IDENTIFICATION DIVISION.
	   PROGRAM-ID. MENU.
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
        
           
       77 WS-OPCAO PIC X.
           88 E-CLIENTE                  VALUE IS "1".
           88 E-VENDEDOR                 VALUE IS "2".
           88 E-RELCLI                   VALUE IS "3".
           88 E-RELVEN                   VALUE IS "4".
		   88 E-INTEGRA                  VALUE IS "5".
           88 E-ENCERRAR                 VALUE IS "X" "x".
       
           88 E-SIM VALUES ARE "S" "s".
           
       01  WS-MESSAGE                    PIC X(050) VALUE SPACES.    
             
       SCREEN SECTION.
	   COPY "TELMENU.SCR".
       LINKAGE SECTION.
	   01  LK-RETORNO-INTEGRA        PIC X(030).
       PROCEDURE DIVISION USING LK-RETORNO-INTEGRA.
       1000-INICIO.
           
           PERFORM UNTIL E-ENCERRAR
              
               MOVE SPACES TO WS-OPCAO
               DISPLAY TELA
               ACCEPT  TELA
               EVALUATE TRUE
                   WHEN E-CLIENTE
				   
                        CALL "CLIENTES" 
						
                   WHEN E-VENDEDOR
				   
                        CALL "VENDEDOR"     
						
                   WHEN E-RELCLI
				   
                        CALL "OCLIREL"   
						
                   WHEN E-RELVEN
				   
				        CALL "OVENREL" 
						
				   WHEN E-INTEGRA
				        PERFORM 
                        
						
               END-EVALUATE
           END-PERFORM.
		   
	   2000-INTEGRA-VENDEDOR.
	   
	       MOVE SPACES            TO LK-RETORNO-INTEGRA
		   CALL "INTEGRA"  USING     LK-RETORNO-INTEGRA
		   IF LK-RETORNO-INTEGRA EQUAL "SPACES"
		       
			  DISPLAY MESSAGE BOX
			  "INTEGRAÇÃO DE VENDEDORES EFETUADA COM SUCESSO !"
		   ELSE
		      STRING "ERRO ROT. INTEGRAÇÃO DE VENDEDORES "
			          LK-RETORNO-INTEGRA
                      DELIMITED BY SIZE
                 INTO WS-MESSAGE
			  END-STRING
			  DISPLAY MESSAGE BOX WS-MESSAGE
		   END-IF.
	       
		   
       3000-FINALIZA.
           
           STOP RUN.
           
       
       