:-include('pokemon_data.pl').
% ali batir
% 2015400261
% compiling: yes 
% complete: no
% ********************************************************************************************************************
% 4.1
% Example: find_pokemon_evolution(40, charmeleon, EvolvedPokemon).
%  Output: EvolvedPokemon = charizard.
% Documentation: find_pokemon_evolution(+PokemonLevel, +Pokemon, -EvolvedPokemon)

     % evolve_two_times_if_PokemonLevel_bigger_than_L1_and_L2
find_pokemon_evolution(PokemonLevel,Pokemon,EvolvedPokemon):- pokemon_evolution(Pokemon,Z,L1),
                       PokemonLevel>=L1, pokemon_evolution(Z,EvolvedPokemon,L2),PokemonLevel>=L2,!. 
find_pokemon_evolution(PokemonLevel,Pokemon,EvolvedPokemon):- pokemon_evolution(Pokemon, EvolvedPokemon, L), PokemonLevel>=L,!.
find_pokemon_evolution(PokemonLevel,Pokemon,EvolvedPokemon):- EvolvedPokemon=Pokemon.  % there_is_no_evolution

 % ********************************************************************************************************************
% 4.2
% Example: pokemon_level_stats(30, squirtle, PokemonHp, PokemonAttack, PokemonDefense).
%  Output: PokemonHp = 104,
%          PokemonAttack = 78,
%          PokemonDefense = 95.
% Documentation: pokemon_level_stats(+PokemonLevel, ?Pokemon, -PokemonHp, -PokemonAttack, -PokemonDefense)

     % get_pokemon_data_and_make_arithmetical_operations
pokemon_level_stats(PokemonLevel, Pokemon, PokemonHp, PokemonAttack, PokemonDefense):-
                    pokemon_stats(Pokemon, _, Hp, Attack, Defense),PokemonHp is Hp+PokemonLevel*2,
                    PokemonAttack is Attack+PokemonLevel*1,PokemonDefense is Defense+PokemonLevel*1.

% ********************************************************************************************************************
% 4.3  
% eksik_var_defendir_type_i_vermiyor
% Example: single_type_multiplier(fire, grass, Multiplier).
%  Output: Multiplier = 2.0;
% Documentation: single_type_multiplier(?AttackerType, ?DefenderType, ?Multiplier)

%List kullancaz listenin içinden eleman seçmek,Listenin eleman numarası ile type i eşitlemek 

%  Multiplier={null},
single_type_multiplier(AttackerType, DefenderType, Multiplier):-type_chart_attack(AttackerType,TypeMultipliersList),
        pokemon_types(List),indexOf(List,DefenderType, R), %savunma yapanın indexini Pokemon types listesinden almak
        nth1(R, TypeMultipliersList, Multiplier),nth1(R, List, DefenderType). %Multiplierlar içinden verilen indexin multiplerini bulma
 

      % This function finds the index of given element in the list
      indexOf([E|_], E, 1).
      indexOf([_|T], E, I) :- indexOf(T, E, I2), I is I2 + 1.

% ********************************************************************************************************************
% 4.4
% Example: type_multiplier(ice, [grass, ground], Multiplier).
%  Output: Multiplier = 4.0;
% Documentation: type_multiplier(?AttackerType, +DefenderTypeList, ?Multiplier)
% type_multiplier(_,[],_).
type_multiplier(AttackerType, DefenderTypeList, Multiplier):- 
     nth1(1, DefenderTypeList, Defender1),single_type_multiplier(AttackerType, Defender1, Multiplier1),
     nth1(2, DefenderTypeList, Defender2),single_type_multiplier(AttackerType, Defender2, Multiplier2),
     Multiplier is (Multiplier1*Multiplier2).

% 4.5
% Example: pokemon_type_multiplier(bulbasaur, geodude, Multiplier).
%  Output: Multiplier = 4.0;
% Documentation: pokemon_type_multiplier(?AttackerPokemon, ?DefenderPokemon, ?Multiplier)
% 
% ama bazı pokemonlar 1 type a sahip o yuzden hata alıon
 pokemon_type_multiplier(AttackerPokemon, DefenderPokemon, Multiplier):-
        pokemon_stats(AttackerPokemon, AttackerTypeList,_,_,_),pokemon_stats(DefenderPokemon, DefenderTypeList,_,_,_),
         length(DefenderTypeList,Def_L),length(AttackerTypeList,Att_L),
         (Att_L=2 -> nth1(1, AttackerTypeList, Attacker1),nth1(2, AttackerTypeList, Attacker2);
         	nth1(1, AttackerTypeList, Attacker1)), % 1 attacker varsa
         %EGER_SALDIRI_VE_DEFENSE_TYPE_SAYISI_1_ISE
         ((Att_L=1 , Def_L=1)->
            (nth1(1, DefenderTypeList, Defender1),single_type_multiplier(Attacker1, Defender1, Multiplier));
        	%eger defense_type_2_ise
         (Att_L=2 , Def_L=2)->
        	(type_multiplier(Attacker1, DefenderTypeList, Multiplier1),type_multiplier(Attacker2, DefenderTypeList, Multiplier2),
        	(Multiplier1 >= Multiplier2 -> Multiplier is Multiplier1; Multiplier is Multiplier2));
         (Att_L=2 , Def_L=1)->
            (nth1(1, DefenderTypeList, Defender1),single_type_multiplier(Attacker1, Defender1, Multiplier1),
             single_type_multiplier(Attacker2, Defender1, Multiplier2),
             (Multiplier1 >= Multiplier2 -> Multiplier is Multiplier1; Multiplier is Multiplier2));
          (Att_L=1 , Def_L=2)->
          	 (type_multiplier(Attacker1, DefenderTypeList, Multiplier))).
  
  

% ********************************************************************************************************************
% 4.6
% Example: pokemon_attack(pikachu, 15, ekans, 12, Damage).
%  Output: Damage = 10.375.
% Documentation: pokemon_attack(+AttackerPokemon, +AttackerPokemonLevel, +DefenderPokemon,
%                               +DefenderPokemonLevel, -Damage)

pokemon_attack(AttackerPokemon, AttackerPokemonLevel, DefenderPokemon, DefenderPokemonLevel, Damage):-
              pokemon_level_stats(AttackerPokemonLevel, AttackerPokemon, _, AttackerPokemonAttack, _), % attackpokemondamage bul
              pokemon_level_stats(DefenderPokemonLevel, DefenderPokemon, _, _, DefenderPokemonLevelDefense), % defensepokemonpoint bul
              pokemon_type_multiplier(AttackerPokemon, DefenderPokemon, TypeMultiplier), % multiplier bul
              Damage is ((0.5*AttackerPokemonLevel*(AttackerPokemonAttack/DefenderPokemonLevelDefense)*TypeMultiplier)+1). % hesapla
              


% ********************************************************************************************************************
% 4.7
% Example: pokemon_fight(pikachu, 15, ekans, 12, Pokemon1Hp, Pokemon2Hp, Rounds).
%  Output: Pokemon1Hp = 11.872727272727257,
%          Pokemon2Hp = -3.25,
%              Rounds = 6.
% Documentation: pokemon_fight(+Pokemon1, +Pokemon1Level, +Pokemon2, +Pokemon2Level,-Pokemon1Hp, -Pokemon2Hp, -Rounds)

  pokemon_fight(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp,Rounds):-
    pokemon_level_stats(Pokemon1Level,Pokemon1,P1hp,_,_), % pokemonların_leveline_göre_değerlerini_aldık_hp_attack_defense
    pokemon_level_stats(Pokemon2Level,Pokemon2,P2hp,_,_),
    pokemon_attack(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Damage1),  % birici pokemonun ikinciye verdiği damage_D1
    pokemon_attack(Pokemon2, Pokemon2Level, Pokemon1, Pokemon1Level, Damage2),  %ikinci pokemonun birinciye verdiği damage_D2
    loop(P1hp,P2hp,Damage1,Damage2,0,Rounds,Pokemon1Hp,Pokemon2Hp).

   loop(Ohp1, Ohp2, Damage1, Damage2,Count,Rounds,Pokemon1Hp,Pokemon2Hp) :- Ohp1>0,Ohp2>0 -> (Hp1 is Ohp1-Damage2, Hp2 is Ohp2-Damage1,
   C is Count+1,loop(Hp1, Hp2, Damage1, Damage2,C,Rounds,Pokemon1Hp,Pokemon2Hp));Rounds is Count,Pokemon1Hp is Ohp1,Pokemon2Hp is Ohp2.
   %loop oluşturdum Rounds hesapladım damage sonrası kalan canları döndürdüm
  

% ********************************************************************************************************************
% 4.8
% Example: pokemon_tournament(ash, teamrocket, WinnerTrainerList).
%  Output: WinnerTrainerList = [ash, team rocket, team rocket, ash].
% Documentation: pokemon_tournament(+PokemonTrainer1, +PokemonTrainer2, -WinnerTrainerList)

     % 1pokemon_trainer ların pokemonlarını ve levellerini al al
     % pokemonteam1den birincisini al pokemonteam2den birincisi al ikisini figh a gönder
     % böyle böyle 4 pokemonu da dövüştür 
     % birinci sağlıgı ikincisinin saglıgından buyuk eşitse winnertrainerListe ekle değilse ikinciyi ekle
     % winnertrainerList i döndür

        pokemon_tournament(PokemonTrainer1, PokemonTrainer2, WinnerTrainerList):-
        pokemon_trainer(PokemonTrainer1,PokemonTeam1,PokemonLevel1),
        pokemon_trainer(PokemonTrainer2,PokemonTeam2,PokemonLevel2),
        pokemon_t(PokemonTrainer1,PokemonTeam1,PokemonLevel1,PokemonTrainer2,PokemonTeam2,PokemonLevel2,
        	Win_Trainer_List,WinnerTrainerList).
        
      pokemon_t(PokemonTrainer1,[H_p1|PokemonTeam1Tail], [H_p1L|PokemonLevel1Tail], PokemonTrainer2,
      	[H_p2|PokemonTeam2Tail], [H_p2L|PokemonLevel2Tail], Win_Trainer_List,WinnerTrainerList):-
      % evolution yap
      find_pokemon_evolution(H_p1L, H_p1, EvolvedPokemon1),find_pokemon_evolution(H_p2L, H_p2, EvolvedPokemon2),

     pokemon_fight(EvolvedPokemon1, H_p1L, EvolvedPokemon2, H_p2L, Pokemon1Hp, Pokemon2Hp, _),
     (Pokemon1Hp >= Pokemon2Hp -> add_tail([],PokemonTrainer1,Win_Trainer_List); add_tail([],PokemonTrainer2,Win_Trainer_List)),
       % winnertrainer boş degil diye hata verebilir

      pokemon_t_2(PokemonTrainer1, PokemonTeam1Tail,PokemonLevel1Tail,
      	PokemonTrainer2,PokemonTeam2Tail,PokemonLevel2Tail,Win_Trainer_List,WinnerTrainerList),!.
     
      pokemon_t_2(_,[],[],_,[],[],X,WinnerTrainerList):-appendlist([],X,WinnerTrainerList),!.
      pokemon_t_2(PokemonTrainer1,[H_p1|PokemonTeam1Tail], [H_p1L|PokemonLevel1Tail],
       PokemonTrainer2, [H_p2|PokemonTeam2Tail], [H_p2L|PokemonLevel2Tail], TrainerList,WinnerTrainerList):-
      % evolution yap
      find_pokemon_evolution(H_p1L, H_p1, EvolvedPokemon1),find_pokemon_evolution(H_p2L, H_p2, EvolvedPokemon2),
       pokemon_fight(EvolvedPokemon1, H_p1L, EvolvedPokemon2, H_p2L, Pokemon1Hp, Pokemon2Hp, _),
       (Pokemon1Hp >= Pokemon2Hp -> appendlist(TrainerList,[PokemonTrainer1],X); appendlist(TrainerList,[PokemonTrainer2],X)),
       pokemon_t_2(PokemonTrainer1,PokemonTeam1Tail,PokemonLevel1Tail,
       	PokemonTrainer2, PokemonTeam2Tail,PokemonLevel2Tail,X,WinnerTrainerList),!.
    

               %pokemon_t ye trainerlarıda gönder 
               %>= ifinden sonra pokemonu değil pokemontrainer1 veya pokemontrainer2 i ekle
          %pokemonların trainerini bul 
         %evolved pokemonları kontrol et evolvedpokemon1


        % add_tail(+List,+Element,-List)
        % Add the given element to the end of the list, without using the append predicate.
        add_tail([],X,[X]).
        add_tail([H|T],X,[H|L]):-add_tail(T,X,L).

         % concatenation of lists
        appendlist([], List, List).
        appendlist([Head|Tail], List, [Head|Rest]) :- appendlist(Tail, List, Rest).

% ********************************************************************************************************************
% 4.9
% Example: best_pokemon(charizard, 42, RemainingHP, BestPokemon).
%  Output: RemainingHP = 144.17441860465118,
%           BestPokemon = golem.

% Documentation: best_pokemon(+EnemyPokemon, +LevelCap, -RemainingHP, -BestPokemon)

     %her pokemon enemy pokemonla fight a girecek 
     %Hpsi en çok olanı alcaksan hem hpi hem de pokemonunadını dondurcen 

  best_pokemon(EnemyPokemon, LevelCap,Reman_hp,Bes_pk):-
          best_pokemon_2(EnemyPokemon, LevelCap, [],ThisList), \+is_empty(ThisList),
          loopforbigger([], ThisList, R_hp, B_Pokemon), 
          best_pokemon_3(R_hp,B_Pokemon,0,Reman_hp,Bes_pk).

 best_pokemon_3(R_hp,B_Pokemon,Zero,Reman_hp,Bes_pk):-
 R_hp>Zero -> (Reman_hp is R_hp, Bes_pk=B_Pokemon).

      
        loopforbigger(Out,ThisList,R_hp,B_Pokemon):-nth1(1, ThisList, Out), nth1(2, Out, R_hp),
        nth1(1, Out, B_Pokemon).

        best_pokemon_2(EnemyPokemon, LevelCap, Out,ThisList):-pokemon_stats(AttackerPokemon,_,_,_,_),
        pokemon_fight(AttackerPokemon, LevelCap, EnemyPokemon, LevelCap, Pokemon1Hp, Pokemon2Hp, _),
        findall([AttackerPokemon,Pokemon1Hp],(Pokemon1Hp>Pokemon2Hp),List1),appendlist(Out,List1,ThisList).
       
         % find empty list
         is_empty(List):-not(member(_,List)). 


% ********************************************************************************************************************
% 4.10  
% Example: best_pokemon_team(team rocket, PokemonTeam)
%   Output: PokemonTeam = [wigglytuff, mew, zapdos, golem].

% Documentation: best_pokemon_team(+OpponentTrainer, -PokemonTeam)
% ********************************************************************************************************************
% 4.11

% Example: pokemon types([grass, flying, ground], [bulbasaur, charmander, charizard, gyarados, pikachu] , PokemonList).
%  Output: PokemonList = [bulbasaur, charizard, gyarados].
% Documentation: pokemon types(+TypeList, ?InitialPokemonList, -PokemonList)

pokemon_types(TypeList, InitialPokemonList, PokemonList):-
   findall(Pokemon,(member(Pokemon, InitialPokemonList), pokemon_types_2(TypeList, Pokemon)), PokemonList).

pokemon_types_2([H|TypeListTail], Pokemon):-
   pokemon_stats(Pokemon, PokemonTypeList, _, _, _),	
   ((member(H, PokemonTypeList), !); pokemon_types_2(TypeListTail, Pokemon)).

% ********************************************************************************************************************
% 4.12 


% Example: generate team([dragon, fire, ghost, ice], [flying, ground, rock], a, 4, PokemonTeam).
% PokemonTeam = [[cloyster, 50, 95, 180], [magmar, 65, 95, 57], [lapras, 130, 85, 80],[dragonair, 61, 84, 65]].
% Documentation: generate_pokemon_team(+LikedTypes, +DislikedTypes, +Criterion, +Count,-PokemonTeam)