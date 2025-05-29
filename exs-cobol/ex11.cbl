       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO11.
       AUTHOR. ALBERT.
       DATE-WRITTEN. 16-05-2025.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 INDICE         PIC 9 VALUE 1.
       01 GRAUS          PIC 999V99.
       01 RADIANO        PIC 999V9999.
       01 RESULTADO      PIC S9(5)V9(5).
       01 OPCAO-TEXTO    PIC X.
       01 RESP_CONTINUA  PIC X VALUE 'S'.
       01 QUANTIDADE     PIC 9 VALUE 0.

       01 TABELA-ANG.
          05 VETOR-ANGULO OCCURS 10 TIMES.
             10 VALOR-ANG PIC 999V99.

       01 MENU-TEXTO.
          05 TEXTO1 PIC X(30) VALUE "1 - SENO".
          05 TEXTO2 PIC X(30) VALUE "2 - COSSENO".
          05 TEXTO3 PIC X(30) VALUE "3 - TANGENTE".

       PROCEDURE DIVISION.

       INICIO.
           PERFORM UNTIL RESP_CONTINUA NOT = 'S'
               MOVE 1 TO QUANTIDADE
               PERFORM ENTRADA-ANGULOS
               PERFORM MOSTRAR-MENU
               DISPLAY "RESULTADOS:"
               PERFORM FAZER-CALCULO
               PERFORM PERGUNTAR-CONTINUAR
           END-PERFORM
           STOP RUN.

       ENTRADA-ANGULOS.
           DISPLAY "QUANTOS ANGULOS (MAX 10)?"
           ACCEPT QUANTIDADE
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > QUANTIDADE
               DISPLAY "ENTRE COM ANGULO EM GRAUS:"
               ACCEPT GRAUS
               MOVE GRAUS TO VETOR-ANGULO(INDICE)
           END-PERFORM.

       MOSTRAR-MENU.
           DISPLAY TEXTO1
           DISPLAY TEXTO2
           DISPLAY TEXTO3
           DISPLAY "ESCOLHA (1/2/3):"
           ACCEPT OPCAO-TEXTO.

       FAZER-CALCULO.
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > QUANTIDADE
               MOVE VETOR-ANGULO(INDICE) TO GRAUS
               COMPUTE RADIANO = GRAUS * 3.1416 / 180
               EVALUATE OPCAO-TEXTO
                   WHEN '1'
                       COMPUTE RESULTADO = FUNCTION SIN(RADIANO)
                       DISPLAY "SENO DE " GRAUS " = " RESULTADO
                   WHEN '2'
                       COMPUTE RESULTADO = FUNCTION COS(RADIANO)
                       DISPLAY "COSSENO DE " GRAUS " = " RESULTADO
                   WHEN '3'
                       COMPUTE RESULTADO = FUNCTION TAN(RADIANO)
                       DISPLAY "TANGENTE DE " GRAUS " = " RESULTADO
                   WHEN OTHER
                       DISPLAY "OPCAO INVALIDA."
               END-EVALUATE
           END-PERFORM.

       PERGUNTAR-CONTINUAR.
           DISPLAY "CONTINUAR? (S/N):"
           ACCEPT RESP_CONTINUA
           IF RESP_CONTINUA NOT = 'S' AND NOT = 'N'
               MOVE 'N' TO RESP_CONTINUA.
