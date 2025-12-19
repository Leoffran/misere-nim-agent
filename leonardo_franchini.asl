/* --------------------------------------------------------------------------
   Aluno: Leonardo Fonseca Franchini
   Estratégia: Misere Nim (ver vídeos de referência)
   Referências:
   https://www.youtube.com/watch?v=LSliURTIL8U
   https://www.youtube.com/watch?v=SUh8C387BVU
   -------------------------------------------------------------------------- */

!start.

+!start : true <- .print("Agente Nim pronto.").
+start : .my_name(Me) & .term2string(Me, MeStr) & player(MeStr) <- .print("Sou jogador Leonardo.").

// ativa quando for o turno
+round(N, WhoPlays)
    : .my_name(Me) & .term2string(Me, MeStr) & MeStr == WhoPlays

    <- .print("Round ", N);
       
       /* COLETAS E FILTROS */
       
       /* Pega todas as torres com pedras */
       .findall(pair(T,S), (tower(T,S) & S > 0), AllPairs);
       
       /* Pega todas as torres com mais de uma pedra */
       .findall(pair(T,S), (tower(T,S) & S > 1), BigPairs);
       .length(BigPairs, CountBig);
       
       /* Pega todas as torres com apenas uma pedra */
       .findall(pair(T,S), (tower(T,S) & S == 1), OnePairs);
       .length(OnePairs, CountOnes);
       
       /* Pega todos os tamanhos para XOR */
       .findall(Sz, .member(pair(_,Sz), AllPairs), AllSizes);
       !calc_xor_total(AllSizes, 0, XorVal);
       
       .print("Status: Big=", CountBig, " Ones=", CountOnes, " XOR=", XorVal);
       
       /* Depois de pegar todas as informações, decide a jogada */
       !decidir_jogada(CountBig, CountOnes, XorVal, BigPairs, OnePairs, AllPairs).


/* =======================================================================
   LÓGICA DE DECISÃO (Misère Nim)
   ======================================================================= */

/* CASO 1) Só existe torres de tamanho 1 */
/* CountBig == 0 */
+!decidir_jogada(0, CountOnes, _, _, OnePairs, _)
    <- .print(">> Modo Endgame");
       /* Retira de qualquer uma delas */
       .nth(0, OnePairs, pair(T, S));
       .print("Jogando na torre ", T, " (tira ", S, ")");
       play(T, S).

/* CASO 2) Só existe uma torre grande */
/* CountBig == 1. */
+!decidir_jogada(1, CountOnes, _, BigPairs, _, _)
    <- .print(">> Modo Trap");
       /* Pega a torre grande */
       .nth(0, BigPairs, pair(BigT, BigS));
       
       /* Verifica paridade das torres de 1 (CountOnes) */
       !executar_trap(BigT, BigS, CountOnes).

/* Trap Auxiliares */
+!executar_trap(T, S, Ones)
    : (Ones mod 2) == 0 /* Se Ones é PAR -> Precisamos de +1 torre de 1 */
    <- R = S - 1;       /* Reduz a torre grande para tamanho 1 */
       .print("Trap: Deixando 1 na torre ", T, " (Tira ", R, ")");
       play(T, R).

+!executar_trap(T, S, Ones) /* Se Ones é IMPAR -> Paridade ja esta boa */
    <- .print("Trap: Removendo torre ", T, " inteira.");
       play(T, S).      /* Remove a torre grande inteira */


/* CASO 3) JOGO NORMAL (Várias torres grandes) */
/* CountBig > 1. Tentar zerar o Nim-Sum (XOR). */
+!decidir_jogada(CountBig, _, XorVal, _, _, AllPairs)
    : CountBig > 1 & XorVal \== 0
    <- .print(">> Modo Normal (Buscando XOR 0)");
       !tentar_zerar_xor(AllPairs, XorVal).

/* CASO 4) SEM ESPERANÇA (XOR já é 0) */
/* Não há jogada vencedora matemática. Joga na primeira disponível. */
+!decidir_jogada(CountBig, _, 0, _, _, AllPairs)
    : CountBig > 1
    <- .print(">> Losing Position (XOR 0). Jogando seguro.");
       .nth(0, AllPairs, pair(T, _));
       .print("Fallback Seguro: Torre ", T, " tira 1");
       play(T, 1).


/* =======================================================================
   RECUPERAÇÃO DE FALHA (Safety Net)
   Se qualquer logica acima falhar (crash), este plano é chamado automaticamente.
   ======================================================================= */

-!decidir_jogada(_, _, _, _, _, AllPairs)
    <- .print("!!! ERRO CRITICO NA LOGICA !!! Ativando Safety Net.");
       .nth(0, AllPairs, pair(T, S)); /* Pega a primeira torre que existe */
       .print("Safety Play: Torre ", T, " tira 1");
       play(T, 1).


/* =======================================================================
   Auxiliares Matemáticos
   ======================================================================= */

/* Busca recursiva para zerar XOR */
+!tentar_zerar_xor([], _) 
    <- .fail. /* Força a falha para cair no -!decidir_jogada se nao achar */

+!tentar_zerar_xor([pair(T,S)|Resto], XorVal)
   /* calcula o numero que a torre deve ficar para zerar o nim sum*/
    <- !xor_calc(S, XorVal, Target);
       /* verifica se é possível aplicar essa jogada na torre */
       !check_xor_move(T, S, Target, Resto, XorVal).

+!check_xor_move(T, S, Target, _, _)
      /* verifica se é possível reduzir uma torre a S*/
    : Target < S
    /* se sim: calcula quantas pedras remover */
    <- Remove = S - Target;
       .print("Jogada XOR: Torre ", T, " tira ", Remove);
       /* realiza jogada */
       play(T, Remove).

+!check_xor_move(_, _, _, Resto, XorVal)
    /* caso não seja possível jogar na torre atual, passa para a próxima da lista */
    /* chama novamente !tentar_zerar_xor */
    <- !tentar_zerar_xor(Resto, XorVal).

/* Calculadora XOR Bit a Bit */

/* caso base: lista vazia -> retorna o acumulador */
+!calc_xor_total([], Acc, Acc).

/* calcula o novo acumulador usando o primeiro da lista (H) */
+!calc_xor_total([H|T], Acc, Res) 
    <- !xor_calc(H, Acc, NewAcc); 
       /* chama recursivamente com o resto da lista e o novo acumulador */
       !calc_xor_total(T, NewAcc, Res).

/* chama xor_loop pra calcular o XOR bit a bit entre A e B */
+!xor_calc(A, B, Res) <- !xor_loop(A, B, 1, 0, Res).

/* caso base: quando A e B são zero -> retorna Acc como resultado final */
+!xor_loop(0, 0, _, Acc, Acc).

+!xor_loop(A, B, Pow, Acc, Res)
   /* pega o bit menos significativo de A e B */
    <- B1 = A mod 2; B2 = B mod 2;
       /* calcula o XOR */
       Bit = (B1 + B2) mod 2;
       /* adiciona no acumulador */
       NewAcc = Acc + (Bit * Pow);
       /* avança para o próximo bit */
       NA = A div 2; NB = B div 2; NP = Pow * 2;
       /* chama recursivamente */
       !xor_loop(NA, NB, NP, NewAcc, Res).