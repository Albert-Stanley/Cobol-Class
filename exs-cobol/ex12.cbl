       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO12.
       AUTHOR. ALBERT.
       DATE-WRITTEN. 23-05-2025.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 INDICE                        PIC 9 VALUE 1.
       01 OPCAO-CALCULO                 PIC X.
       01 RESPOSTA-CONTINUAR            PIC X VALUE 'S'.

       01 BASE-TRIANGULO                PIC 999V99.
       01 ALTURA-TRIANGULO              PIC 999V99.
       01 LADO1-TRIANGULO               PIC 999V99.
       01 LADO2-TRIANGULO               PIC 999V99.
       01 LADO3-TRIANGULO               PIC 999V99.
       01 RAIO-CIRCULO                  PIC 999V99.

       01 AREA-CALCULADA                PIC 9999V9999.
       01 PERIMETRO-CALCULADO           PIC 9999V9999.

       01 TABELA-ENTRADA.
          05 VALORES-ENTRADA OCCURS 6 TIMES.
             10 VALOR-ENTRADA           PIC 999V99.

       01 MENSAGEM-OPCOES.
          05 TEXTO1 PIC X(40)
          VALUE "1 - Triangulo escaleno (area e perimetro)".
          05 TEXTO2 PIC X(40) VALUE "2 - CIrculo (area)".

       PROCEDURE DIVISION.

       INICIO.
           PERFORM UNTIL RESPOSTA-CONTINUAR NOT = 'S'
               PERFORM MOSTRAR-MENU
               PERFORM ENTRADA-DADOS
               PERFORM CALCULAR-RESULTADO
               PERFORM PERGUNTAR-CONTINUAR
           END-PERFORM
           STOP RUN.

       MOSTRAR-MENU.
           DISPLAY "ESCOLHA A OPÇÃO DE CÁLCULO:"
           DISPLAY TEXTO1
           DISPLAY TEXTO2
           DISPLAY "DIGITE 1 OU 2:"
           ACCEPT OPCAO-CALCULO.

       ENTRADA-DADOS.
           EVALUATE OPCAO-CALCULO
               WHEN '1'
                   DISPLAY "ENTRE COM A BASE DO TRIANGULO:"
                   ACCEPT VALOR-ENTRADA(1)
                   MOVE VALOR-ENTRADA(1) TO BASE-TRIANGULO

                   DISPLAY "ENTRE COM A ALTURA DO TRIANGULO:"
                   ACCEPT VALOR-ENTRADA(2)
                   MOVE VALOR-ENTRADA(2) TO ALTURA-TRIANGULO

                   DISPLAY "ENTRE COM O LADO 1:"
                   ACCEPT VALOR-ENTRADA(3)
                   MOVE VALOR-ENTRADA(3) TO LADO1-TRIANGULO

                   DISPLAY "ENTRE COM O LADO 2:"
                   ACCEPT VALOR-ENTRADA(4)
                   MOVE VALOR-ENTRADA(4) TO LADO2-TRIANGULO

                   DISPLAY "ENTRE COM O LADO 3:"
                   ACCEPT VALOR-ENTRADA(5)
                   MOVE VALOR-ENTRADA(5) TO LADO3-TRIANGULO
               WHEN '2'
                   DISPLAY "ENTRE COM O RAIO DO CÍRCULO:"
                   ACCEPT VALOR-ENTRADA(6)
                   MOVE VALOR-ENTRADA(6) TO RAIO-CIRCULO
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA."
           END-EVALUATE.

       CALCULAR-RESULTADO.
           EVALUATE OPCAO-CALCULO
               WHEN '1'
                  COMPUTE AREA-CALCULADA
                  = (BASE-TRIANGULO * ALTURA-TRIANGULO) / 2
                  COMPUTE PERIMETRO-CALCULADO =
                   LADO1-TRIANGULO + LADO2-TRIANGULO + LADO3-TRIANGULO
                  DISPLAY "AREA DO TRIANGULO: " AREA-CALCULADA
                  DISPLAY "PERÍMETRO DO TRIÂNGULO: " PERIMETRO-CALCULADO
               WHEN '2'
           COMPUTE AREA-CALCULADA = 3.1416 * RAIO-CIRCULO * RAIO-CIRCULO
                   DISPLAY "AREA DO CIRCULO: " AREA-CALCULADA
               WHEN OTHER
                   DISPLAY "NENHUM CALCULO REALIZADO."
           END-EVALUATE.

       PERGUNTAR-CONTINUAR.
           DISPLAY "DESEJA FAZER OUTRO CALCULO? (S/N):"
           ACCEPT RESPOSTA-CONTINUAR
           IF RESPOSTA-CONTINUAR NOT = 'S' AND NOT = 'N'
               MOVE 'N' TO RESPOSTA-CONTINUAR.
