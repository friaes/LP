:- [codigo_comum].

%2.1 - extrai_ilhas_linha/3
extrai_ilhas_linha(N_L,Linha,Ilhas):- extrai_ilhas_linha(N_L,Linha,Ilhas,[],1).

extrai_ilhas_linha(_,[],Aux,Aux,_):- !.

extrai_ilhas_linha(N_L,[N|R],Ilhas,Aux,N_C):-
    N > 0, !,
    append(Aux,[ilha(N,(N_L,N_C))],L),
    Novo_N_C is N_C + 1,
    extrai_ilhas_linha(N_L,R,Ilhas,L,Novo_N_C).

extrai_ilhas_linha(N_L,[N|R],Ilhas,Aux,N_C):-
    N =:= 0,
    Novo_N_C is N_C + 1,
    extrai_ilhas_linha(N_L,R,Ilhas,Aux,Novo_N_C).


%2.2 - ilhas/2
ilhas(Puz,Ilhas):- ilhas(Puz,Ilhas,[],1).

ilhas([],Aux,Aux,_).

ilhas([P|R],Ilhas,Aux,N_L):-
    extrai_ilhas_linha(N_L,P,L1),
    Novo_N_L is N_L + 1,
    append(Aux,L1,L2),
    ilhas(R,Ilhas,L2,Novo_N_L).


%2.3 - vizinhas/3
vizinhas(Ilhas,ilha(N,(N_L,N_C)),Vizinhas):-
    subtract(Ilhas,[ilha(N,(N_L,N_C))],L),
    parteL(L,N_L,N_C,Lin_m,Lin_M),
    parteC(L,N_L,N_C,Col_m,Col_M),
    ultimo(Lin_m,U_Lin_m), primeiro(Lin_M,P_Lin_M),
    ultimo(Col_m,U_Col_m), primeiro(Col_M,P_Col_M),
    append([U_Col_m,U_Lin_m,P_Lin_M,P_Col_M],Vizinhas).

parteC([],_,_,[],[]):- !.

parteC([P|R],N_L,N_C,Col_m,Col_M):-
    P = ilha(_,(_,N_C1)),
    N_C \== N_C1, !,
    parteC(R,N_L,N_C,Col_m,Col_M).

parteC([P|R],N_L,N_C,[P|L],Col_M):-
    P = ilha(_,(N_L1,N_C1)),
    N_C == N_C1, N_L1 < N_L,
    parteC(R,N_L,N_C,L,Col_M).

parteC([P|R],N_L,N_C,Col_m,[P|L]):-
    P = ilha(_,(N_L1,N_C1)),
    N_C == N_C1, N_L1 > N_L,
    parteC(R,N_L,N_C,Col_m,L).

parteL([],_,_,[],[]):- !.

parteL([P|R],N_L,N_C,Lin_m,Lin_M):-
    P = ilha(_,(N_L1,_)),
    N_L \== N_L1, !,
    parteL(R,N_L,N_C,Lin_m,Lin_M).

parteL([P|R],N_L,N_C,[P|L],Lin_M):-
    P = ilha(_,(N_L1,N_C1)),
    N_L == N_L1, N_C1 < N_C,
    parteL(R,N_L,N_C,L,Lin_M).

parteL([P|R],N_L,N_C,Lin_m,[P|L]):-
    P = ilha(_,(N_L1,N_C1)),
    N_L == N_L1, N_C1 > N_C,
    parteL(R,N_L,N_C,Lin_m,L).

ultimo(L,U):- length(L,N), N =< 1, !, U = L.
ultimo(L,[U]):- append(_,[U],L).

primeiro(L,P):- length(L,N), N =< 1, !, P = L.
primeiro([P|_],[P]).


%2.4 - estado/2
estado(Ilhas,Estado):- estado(Ilhas,Estado,Ilhas).

estado([],[],_).

estado([P|R],[[P,Vizinhas,[]]|L],Ilhas):-
    vizinhas(Ilhas,P,Vizinhas),
    estado(R,L,Ilhas).


%2.5 - posicoes_entre/3
posicoes_entre((L1,C1),(L2,C2),Posicoes):-
    L1 == L2 ,intervaloC(L1,C1,C2,Posicoes);
    C1 == C2, intervaloL(C1,L1,L2,Posicoes).

intervaloC(L1,C1,C2,Posicoes):-
    C1 < C2 ->  NC1 is C1+1, NC2 is C2-1,
    findall((L1,X),between(NC1,NC2,X),Posicoes);
    intervaloC(L1,C2,C1,Posicoes).

intervaloL(C1,L1,L2,Posicoes):-
    L1 < L2 ->  NL1 is L1+1, NL2 is L2-1,
    findall((X,C1),between(NL1,NL2,X),Posicoes);
    intervaloL(C1,L2,L1,Posicoes).


%2.6 - cria_ponte/3
cria_ponte((L1,C1),(L2,C2),Ponte):-
    L1 == L2, C1 > C2, !, cria_ponte((L2,C2),(L1,C1),Ponte);
    C1 == C2, L1 > L2, !, cria_ponte((L2,C2),(L1,C1),Ponte).

cria_ponte((L1,C1),(L2,C2),Ponte):-
    L1 == L2, Ponte = ponte((L1,C1),(L2,C2));
    C1 == C2, Ponte = ponte((L1,C1),(L2,C2)).


%2.7 - caminho_livre/5
caminho_livre(Pos1, Pos2, Posicoes, ilha(_,X), ilha(_,Y)):-
    Pos1 = X, Pos2 = Y, !;
    Pos2 = X, Pos1 = Y, !;
    posicoes_entre(X, Y, Pos_entre),
    length(Posicoes,N), subtract(Posicoes,Pos_entre,L),
    length(L,N).


%2.8 - actualiza_vizinhas_entrada/5
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I,Vz,P], [I,Nova_Vz,P]):-
    findall(X, (member(X, Vz), caminho_livre(Pos1, Pos2, Posicoes, I, X)), Nova_Vz).


%2.9 - actualiza_vizinhas_apos_pontes/4
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado):-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_estado).


%2.10 - ilhas_terminadas/2
ilhas_terminadas([],[]).
ilhas_terminadas([[ilha(N_pontes,Pos), _, Pontes]|R], [ilha(N_pontes,Pos)|L]):-
    N_pontes \== 'X', length(Pontes, N_pontes), !, ilhas_terminadas(R,L).

ilhas_terminadas([_|R], Ilhas_term):- ilhas_terminadas(R, Ilhas_term).


%2.11 - tira_ilhas_terminadas_entrada/3
tira_ilhas_terminadas_entrada(Ilhas_term, [I,Vz,P], [I,Nova_Vz,P]):-
    subtract(Vz, Ilhas_term, Nova_Vz).


%2.12 - tira_ilhas_terminadas/3
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


%2.13 - marca_ilhas_termindas_entrada/3
marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_pontes, Pos),Vz,P], Nova_entrada):-
    findall(ilha('X',Pos), (member(X,Ilhas_term), X == ilha(N_pontes, Pos)), Nova_ilha),
    aux_marca_ilhas_terminadas_entrada([ilha(N_pontes, Pos),Vz,P], Nova_entrada, Nova_ilha).

aux_marca_ilhas_terminadas_entrada([ilha(N_pontes, Pos),Vz,P], Nova_entrada, Nova_ilha):-
    length(Nova_ilha,0) -> Nova_entrada = [ilha(N_pontes, Pos),Vz,P];
    Nova_ilha = [I],
    Nova_entrada = [I,Vz,P].


%2.14 - marca_ilhas_terminadas/3
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


%2.15 - trata_ilhas_terminadas/2
trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, Estado1),
    marca_ilhas_terminadas(Estado1, Ilhas_term, Novo_estado).


%2.16 - junta_pontes/5
junta_pontes(Estado, Num_pontes, ilha(N1,Pos1), ilha(N2,Pos2), Novo_estado):-
    cria_ponte(Pos1, Pos2, Ponte),
    length(Pontes, Num_pontes), maplist(=(Ponte), Pontes),
    maplist(aux_junta_pontes(ilha(N1,Pos1), ilha(N2,Pos2), Pontes), Estado, Estado_pontes),
    actualiza_vizinhas_apos_pontes(Estado_pontes, Pos1, Pos2, Estado_vizinhas),
    trata_ilhas_terminadas(Estado_vizinhas, Novo_estado).

aux_junta_pontes(Ilha1, Ilha2, Pontes, [I,Vz,P1], [I,Vz,P2]):-
    I == Ilha1, P2 = Pontes, !;
    I == Ilha2, P2 = Pontes, !;
    P2 = P1.
