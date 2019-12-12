       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIREL.
       AUTHOR. GERSON GUSSO.
       DATE-WRITTEN. DEZ-2019.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
		   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   COPY "CLIENTES.SL".
	   COPY "SAIDA.SL".
	   COPY "WORK.SL".  
        

	   DATA DIVISION.
	   FILE SECTION.
	   COPY "CLIENTES.FD".
	   COPY "SAIDA.FD".
	   COPY "WORK.FD". 
        
       WORKING-STORAGE SECTION.
       01  STAT-CLIENTES                      PIC XX.
       01  STAT-SAIDA                         PIC XX.	   
	   01  WS-CAMPOS-OK                   PIC X         VALUE SPACES.
	   01  WS-EOF                         PIC X         VALUE SPACES.
	   01  WS-EOF-SD                      PIC X         VALUE SPACES.
	   01  CONTADOR                       PIC 9(005)    VALUE 0.
       01  PAGINA                             PIC 9(004) VALUE 0.
       01  LINHAS                             PIC 9(002) VALUE 80. 
	   01  TRACOS                                        PIC X(080) VALUE ALL "=".
	   01  CAB01.
	       05 PIC X(068)                                 VALUE "RELATORIO DO CADASTRO DE CLIENTES".
	       05 PIC X(008) VALUE "PAGINA: ".
	       05 CAB01-PAGINA PIC ZZZ9.
	   01  CAB02.
	       05 PIC X(007)                                     VALUE " CODIGO".
	       05 PIC X(003).
	       05 PIC X(025)                                     VALUE "CNPJ".
	       05 PIC X(003).
	       05 PIC X(020)                                     VALUE "RAZAO SOCIAL".
               05 PIC X(003).
               05 PIC X(010)                                      VALUE "LOGITUDE".
	       05 PIC X(003).
               05 PIC X(010)                                      VALUE "LATITUDE".
       01  LDT.
           05 PIC X(003).
           05 LDT-CODIGO                                      PIC ZZZZZZ9.
           05 PIC X(003).
           05 LDT-CNPJ                               PIC ZZ.ZZZ.ZZZ-ZZZZ-Z9.
           05 PIC X(003).
           05 LDT-RAZAO                   PIC X(020).
           05 PIC X(003).
           05 LDT-LONGITUDE               PIC -ZZ9V99999999.
		   05 PIC X(003).
           05 LDT-LONGITUDE               PIC -ZZ9V99999999.
       01  RODAPE.
           05 PIC X(010).
           05 PIC X(030) VALUE "TOTAL DE REGISTROS IMPRESSOS: ".
           05 ROD-CONTADOR PIC ZZ.ZZ9. 
       LINKAGE SECTION.
       01 LIN-LIGACAO.
          05 LK-TIPO-ORD                                           PIC X.       
          05 LK-CAMPO                                              PIC X.       
          05 LK-CODIGO                                         PIC 9(007).  
          05 LK-RAZAO                                          PIC X(030).
          05 LK-RETORNO		                               PIC X(030).
       PROCEDURE DIVISION USING LIN-LIGACAO.
       1000-INICIO.
           PERFORM 1000-OPEN-ARQUIVO
	   PERFORM 2000-PROCESSA
	   PERFORM 3000-FINALIZA.
		            
       1000-OPEN-ARQUIVO.
	       OPEN INPUT CLIENTES
	       IF  STAT-CLIENTES NOT EQUAL "00"
	           STRING "ERRO AO ABRIR CLIENTES ! "
                      STAT-CLIENTES
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	           GOBACK
           END-IF.
	2000-PROCESSA.
	    IF  LK-CAMPO EQUAL "C"
		       IF LK-TIPO-ORD EQUAL "A"
		          SORT WORK ON ASCENDING CLI-CODIGO 
			      INPUT PROCEDURE 2000-SORTIN
                  OUTPUT PROCEDURE 2000-SORTOUT
			   ELSE
          			SORT WORK ON DESCENDING CLI-CODIGO 
			        INPUT PROCEDURE 2000-SORTIN
                    OUTPUT PROCEDURE 2000-SORTOUT 
               END-IF				  
            ELSE
	 	       IF LK-TIPO-ORD EQUAL "A"
		          SORT WORK ON ASCENDING CLI-RAZAO 
			      INPUT PROCEDURE 2000-SORTIN 
                  OUTPUT PROCEDURE 2000-SORTOUT 
			   ELSE
          			SORT WORK ON DESCENDING CLI-RAZAO
			        INPUT PROCEDURE 2000-SORTIN 
                    OUTPUT PROCEDURE 2000-SORTOUT  
               END-IF			
                
           END-IF.
      
       2000-SORTIN SECTION.
	       
	    MOVE ZEROS      TO CLI-CODIGO  
	    START CLIENTES KEY IS GREATER THAN CLI-CODIGO
	    IF STAT-CLIENTES NOT EQUAL "00" AND "23"
	       STRING "ERRO NO START COD CLIENTES ! "
      		      	   STAT-CLIENTES
                  	 DELIMITED BY SIZE
                  	 INTO LK-RETORNO
             	END-STRING
	        GOBACK
	     END-IF	
            
            READ  CLIENTES
		   IF STAT-CLIENTES NOT EQUAL "00"
		      STRING "ERRO NO PRIMEIRO READ COD CLIENTES ! "
                  	 STAT-CLIENTES
                   	 DELIMITED BY SIZE
                  	 INTO LK-RETORNO
                      END-STRING
	              GOBACK
		   END-IF	
		   
           PERFORM UNTIL WS-EOF EQUAL WS-EOF = "F"
		      	  
	       IF  STAT-CLIENTES EQUAL "00"
                  MOVE RG-CLIENTES TO SORT-REC 
                  RELEASE SORT-REC 
              END-IF
			  
		  READ  CLIENTES NEXT AT END 
                      MOVE "F" TO WS-EOF	
           END-PERFORM.
		   
           CLOSE CLIENTES
	   IF  STAT-CLIENTES NOT EQUAL "00"
	          STRING "ERRO AO FECHAR CLIENTES ! "
                      STAT-CLIENTES
                   DELIMITED BY SIZE
                   INTO LK-RETORNO
               END-STRING
	       GOBACK
           END-IF.
		   
      2000-SORTIN-EXIT. EXIT.
	   
      2000-SORTOUT  SECTION. 
          OPEN OUTPUT SAIDA
	   IF  STAT-SAIDA NOT EQUAL "00"
               STRING "ERRO AO ABRIR ARQ. SAIDA ! "
                      STAT-SAIDA
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
              END-STRING
              GOBACK
           END-IF. 
		   
	    RETURN  WORK AT END MOVE "F" TO WS-EOF-SD
		   
	    PERFORM UNTIL WS-EOF-SD EQUAL "F"
				
				PERFORM 2001-GERA-RELATORIO
                RETURN  WORK AT END MOVE "F" TO WS-EOF-SD
	    END-PERFORM.
		   
	    PERFORM 2003-RODAPE
		   
	   CLOSE SAIDA
	   IF  STAT-SAIDA NOT EQUAL "00"
	           STRING "ERRO AO FECHAR ARQ. SAIDA ! "
                      STAT-SAIDA
         	        DELIMITED BY SIZE
                	 INTO LK-RETORNO
               	   END-STRING
	           GOBACK
           END-IF. 
		   
	   MOVE SPACES                                     TO LK-RETORNO.
		   
      2001-GERA-RELATORIO.
  
          MOVE SORT-CODIGO            TO LDT-CODIGO
          MOVE SORT-CNPJ              TO LDT-CNPJ
          MOVE SORT-RAZAO             TO LDT-RAZAO
          MOVE SORT-LATITUDE          TO LDT-LATITUDE
          MOVE SORT-LOGITUDE          TO LTD-LONGITUDE
	   IF LINHAS > 61
              PERFORM 2002-CABECALHO
           END-IF
           ADD 1 TO LINHAS, CONTADOR
           WRITE RG-SAIDA FROM LDT AFTER 1.
			
      2002-CABECALHO.
	   
           ADD 1                        TO PAGINA
           MOVE 5                       TO LINHAS
           MOVE PAGINA                  TO CAB01-PAGINA
 
           IF PAGINA = 1
              WRITE RG-SAIDA  FROM TRACOS AFTER 1
           ELSE
               WRITE RG-SAIDA FROM TRACOS AFTER PAGE
           END-IF
		   
           WRITE RG-SAIDA     FROM CAB01 AFTER 1.
           WRITE RG-SAIDA     FROM TRACOS AFTER 1.
           WRITE RG-SAIDA     FROM CAB02 AFTER 1.
           WRITE RG-SAIDA     FROM SPACES AFTER 1.

      2003-RODAPE.
	   
           MOVE CONTADOR      TO ROD-CONTADOR
           WRITE RG-SAIDA     FROM TRACOS AFTER 2.
           WRITE RG-SAIDA     FROM RODAPE AFTER 1. 
	        
      2000-SORTOUT-EXIT. EXIT.
		
      3000-FINALIZA.
  
            GOBACK.  
       		   
