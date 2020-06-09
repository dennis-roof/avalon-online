module Main exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Random
import Debug


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- ALIAS


type alias Player =
  { index: Int
  , name: String
  , isTraitor: Bool
  , selected: Bool
  , vote: Bool
  , failedMissions: Int
  , accuses: Int
  , suspicions: Int
  }


-- HELPER FUNCTIONS


classes : String -> Attribute msg
classes classNames = 
  classList
  ( List.map 
    ( \className -> ( className, True ) ) 
    ( String.split " " classNames )
  )


initPlayers : List Player
initPlayers =
  [ { index = 0, name = "Player", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { index = 1, name = "Mary", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { index = 2, name = "Robert", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { index = 3, name = "Jennifer", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { index = 4, name = "David", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  ]


showRoundText : Int -> String
showRoundText round =
  case round of
    1 -> "Who will you take with you on your mission?"
    2 -> "Vote on the selected team."
    3 -> "Do you wish to point out a possible traitor?"
    4 -> "Choose your mission outcome."
    _ -> "Something went wrong, game ended."


getMaxTeamSize : Int -> Int
getMaxTeamSize mission =
  case mission of
    1 -> 2
    2 -> 3
    3 -> 2
    4 -> 3
    5 -> 3
    _ -> 3


getTeamSize : List Player -> Int
getTeamSize players = List.length ( List.filter ( \player -> player.selected ) players )


aiAssignTeam : Int -> Int -> List Player -> List Player
aiAssignTeam leader mission players =
  let
    isTraitor = List.member True ( List.indexedMap ( \index player -> if player.index == index then player.isTraitor else False ) players )
    selectedIndexes =
      [ leader ] ++
        List.map 
        ( \player -> player.index ) 
        ( List.take 
          ( ( getMaxTeamSize mission ) - 1 ) 
          ( List.sortBy .failedMissions ( List.filter ( \player -> player.index /= leader ) players ) )
        )
    dummy0 = Debug.log "Mission" mission
    dummy1 = Debug.log "Max team size" ( getMaxTeamSize mission )
    dummy2 = Debug.log "selected indexes" selectedIndexes
    dummy3 = Debug.log "leader index" leader
  in
    List.indexedMap ( \index player -> { player | selected = ( List.member index selectedIndexes ) } ) players


aiVoteTeam : Int -> Int -> List Player -> Bool
aiVoteTeam leader playerIndex players =
  let
    selectedPlayers = List.filter ( \player -> player.selected ) players
    hasFailedMissions = List.length ( List.filter ( \player -> player.failedMissions > 0 ) selectedPlayers ) > 0
    teamAverageFailures = 
      toFloat ( List.sum ( List.map ( \player -> player.failedMissions ) selectedPlayers ) ) / toFloat ( List.length selectedPlayers )
    groupAverageFailures = 
      toFloat ( List.sum ( List.map ( \player -> player.failedMissions ) players ) ) / toFloat ( List.length players )
  in
    playerIndex == leader || not hasFailedMissions || ( teamAverageFailures > groupAverageFailures )


aiChoosePossibleTraitor : Int -> List Player -> Int
aiChoosePossibleTraitor playerIndex players =
  List.sum
    ( List.indexedMap
      ( \index player -> if player.index /= playerIndex && index == 0 then player.index else if index == 1 then player.index else 0 ) 
      ( List.reverse ( List.sortBy .failedMissions ( List.filter ( \player -> player.selected ) players ) ) )
    )


updateSuspicions : List Player -> List Int -> List Player
updateSuspicions players suspicionIndexes =
  List.map
  ( \player -> 
    { player
    | suspicions = 
      player.suspicions 
      + List.length ( List.filter ( \index -> index == player.index ) suspicionIndexes )
    } 
  )
  players


aiChooseMissionOutcome : Int -> Model -> Bool
aiChooseMissionOutcome playerIndex model =
  let
    isTraitor = List.member True ( List.indexedMap ( \index player -> if player.index == index then player.isTraitor else False ) model.players )
    numberOfSuspicions = 
      List.sum ( List.indexedMap ( \index player -> if index == playerIndex then player.suspicions else 0 ) model.players )
    totalAverageSuspicions = 
      List.sum ( List.map ( \player -> player.suspicions ) model.players )
  in
    if not isTraitor || model.mission == 1
    then True
    else
      if model.mission == 5
      then False
      else
        ( numberOfSuspicions < totalAverageSuspicions )


updatePlayerOutcomes : Bool -> List Player -> List Player
updatePlayerOutcomes success players =
  List.map
  ( \player -> 
    { player
    | failedMissions =
        if not player.selected 
        then player.failedMissions
        else
          if success then player.failedMissions else player.failedMissions + 1
    , selected = False
    }
  )
  players


assignOutcomes : Model -> List Bool -> Model
assignOutcomes model outcomes =
  let
    success = 
      List.length ( List.filter ( \currentOutcome -> currentOutcome ) outcomes ) 
      > List.length ( List.filter ( \currentOutcome -> not currentOutcome ) outcomes )
    nextLeader = if model.leader + 1 >= List.length model.players then 0 else model.leader + 1
  in
    { model 
      | players =
          if nextLeader == 0
          then updatePlayerOutcomes success model.players
          else aiAssignTeam nextLeader ( model.mission + 1 ) ( updatePlayerOutcomes success model.players )
      , mission = model.mission + 1
      , leader = nextLeader
      , round = if nextLeader == 0 then 1 else 2
      , missionOutcomes = model.missionOutcomes ++ [ success ]
    }


hasGameEnded : Model -> Bool
hasGameEnded model =
  ( List.length ( List.filter ( \outcome -> outcome == True ) model.missionOutcomes ) ) == 3
  || ( List.length ( List.filter ( \outcome -> outcome == False ) model.missionOutcomes ) ) == 3


hasPlayerWon : Model -> Bool
hasPlayerWon model =
  ( List.length 
    ( List.filter 
      ( \outcome -> outcome == ( if model.isTraitor then False else True ) ) 
      model.missionOutcomes 
    )
  ) == 3


-- MODEL


type alias Model =
  { name: String
  , isTraitor: Bool
  , players: List Player
  , mission: Int
  , round: Int
  , leader: Int
  , teamAllowed: Bool
  , voteTrack: Int
  , missionOutcomes: List Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model
    "Player"
    False
    initPlayers
    0
    1
    0
    False
    1
    []
  , Cmd.none
  )


-- UPDATE


type Msg
  = EnterName String
  | InitializeGame
  | AssignRoles (List Int)
  | AssignLeader Int
  | SelectTeamMember Int
  | Vote Bool
  | ChooseTraitor Int
  | ChooseOutcome Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EnterName name ->
      ( 
        { model 
        | name = name
        , players =
          List.indexedMap
          ( \playerIndex player ->
            { player | name = if playerIndex == 0 then model.name else player.name }
          )
          model.players
        }
      , Cmd.none
      )
    
    InitializeGame ->
      ( { model
        | mission = 1
        , round = 1
        , voteTrack = 1
        , missionOutcomes = []
        }
      , Random.generate AssignRoles (Random.list 2 (Random.int 0 4))
      )

    AssignRoles [] ->
      ( model
      , Cmd.none
      )

    AssignRoles ( firstTraitor :: otherTraitor ) ->
      let
        traitorIndexes = firstTraitor :: if [ firstTraitor ] == otherTraitor then [ firstTraitor + 1 ] else otherTraitor
      in
        ( 
          { model
          | isTraitor = List.member 0 traitorIndexes
          , players =
            List.indexedMap
            ( \playerIndex player ->
              { player | isTraitor = List.member playerIndex traitorIndexes }
            )
            model.players
          , mission = 1
          }
        , Random.generate AssignLeader (Random.int 0 4)
        )
    
    AssignLeader leader ->
      ( 
        if leader == 0
        then { model | leader = leader, round = 1 }
        else { model | leader = leader, players = aiAssignTeam leader model.mission model.players, round = 2 }
        , Cmd.none
      )
    
    SelectTeamMember playerIndex ->
      let
        finishedSelection = getTeamSize model.players == ( getMaxTeamSize model.mission ) - 1
        isPlayerInTeam = List.member 0 ( List.map ( \player -> player.index ) ( List.filter ( \player -> player.selected ) model.players ) )
      in
        (
          if not finishedSelection || isPlayerInTeam
          then
            { model 
            | players = 
              List.map ( \player -> if player.index == playerIndex then { player | selected = True } else player ) model.players
            , round = if finishedSelection then 2 else 1
            }
          else
            ( assignOutcomes
              model
              ( List.map 
                ( \player -> aiChooseMissionOutcome player.index model ) 
                ( List.filter 
                  ( \player -> player.selected ) 
                  ( List.map ( \player -> if player.index == playerIndex then { player | selected = True } else player ) model.players )
                )
              )
            )
        , Cmd.none
        )
    
    Vote vote -> 
      let
        votes = vote :: 
          List.map 
          ( \player -> aiVoteTeam model.leader player.index model.players ) 
          ( List.filter ( \player -> player.index > 0 ) model.players )
        success = List.sum ( List.map ( \currentVote -> if currentVote then 1 else 0 ) votes ) > ( toFloat ( List.length votes ) / 2 )
        isPlayerInTeam = List.member 0 ( List.map ( \player -> player.index ) ( List.filter ( \player -> player.selected ) model.players ) )
      in
        ( 
          if not success || isPlayerInTeam
          then
            { model
            | players = List.map2 ( \playerVote player -> { player | vote = playerVote } ) votes model.players
            , teamAllowed = success
            , round = if success then 4 else 3
            , voteTrack = if not success then model.voteTrack + 1 else 0
            }
          else
            assignOutcomes
            { model
            | players = List.map2 ( \playerVote player -> { player | vote = playerVote } ) votes model.players
            , teamAllowed = True
            , round = 1
            , voteTrack = 0
            }
            ( List.map 
              ( \player -> aiChooseMissionOutcome player.index model ) 
              ( List.filter ( \player -> player.selected ) model.players )
            )
        , Cmd.none
        )
    
    ChooseTraitor suspicionIndex -> 
      let
        suspicionIndexes = suspicionIndex ::
          List.map 
          ( \player -> aiChoosePossibleTraitor player.index model.players )
          ( List.filter ( \player -> player.index > 0 ) model.players )
        nextLeader = if model.leader + 1 >= List.length model.players then 0 else model.leader + 1
        nextMission = if model.voteTrack == 6 then model.mission + 1 else model.mission
      in
        ( { model 
          | players =
              if nextLeader == 0
              then updateSuspicions model.players suspicionIndexes
              else aiAssignTeam nextLeader nextMission ( updateSuspicions model.players suspicionIndexes )
          , mission = nextMission
          , leader = nextLeader
          , round = if nextLeader == 0 then 1 else 2
          , voteTrack = if model.voteTrack == 6 then 1 else model.voteTrack
          , missionOutcomes = if model.voteTrack == 6 then model.missionOutcomes ++ [ False ] else model.missionOutcomes
          }
        , Cmd.none
        )
    
    ChooseOutcome outcome -> 
      ( assignOutcomes
        model
        ( outcome :: 
          List.map 
          ( \player -> aiChooseMissionOutcome player.index model )
          ( List.filter ( \player -> player.index > 0 && player.selected ) model.players )
        )
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model = 
  div 
  [ classes "ph5 tc", style "font-family" "'Roboto', sans-serif" ] 
  [ showScreen model 
  , Html.node "link" 
    [ Html.Attributes.rel "stylesheet"
    , Html.Attributes.href "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap" 
    ] 
    []
  , Html.node "link" 
    [ Html.Attributes.rel "stylesheet"
    , Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/tachyons/4.11.1/tachyons.min.css" 
    ] 
    []
  , Html.node "link" 
    [ Html.Attributes.rel "stylesheet"
    , Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" 
    ] 
    []
  ]


showScreen : Model -> Html Msg
showScreen model =
  case model.mission of
    0 -> showStartScreen model
    _ ->
      if hasGameEnded model
      then showEndScreen model
      else showTeamScreen model


showStartScreen : Model -> Html Msg
showStartScreen model =
  div []
    [ h1 [ classes "f2 fw3" ] [ text "Welcome to Avalon" ]
    , p [ classes "" ] [ text "a game of trust and betrayal" ]
    , label [ classes "mr3" ] [ text "Enter your name:" ]
    , input [ placeholder "Player", value model.name, onInput EnterName ] []
    , div [ classes "ma3" ] [ button [ onClick InitializeGame ] [ text "Start" ] ]
    ]


showTeamScreen : Model -> Html Msg
showTeamScreen model =
  div []
    ( 
      List.append
      [ h1 [ classes "f2 fw3" ] [ text ( "Mission " ++ String.fromInt model.mission ++ " out of 5" ) ]
      , if model.isTraitor
        then div []
          [ p [ classes "red fw7" ] [ text "You are a traitor. You can see the other traitors." ]
          , p [] [ text "Fail the majority of missions to win this game. Try to not to be suspected as traitor." ]
          ]
        else
          div [] 
            [ p [ classes "dark-green fw7" ] [ text "You are loyal." ]
            , p [] [ text "Succeed the majority of missions to win this game. Beware of 2 hidden traitors." ]
            ]
      , showMissionStatus model
      , p [ classes "cb" ] 
        [ text
          ( 
            (
              String.concat 
              ( 
                List.map 
                ( \player -> player.name ) 
                ( List.filter ( \player -> player.index == model.leader ) model.players ) 
              )
            ) ++ " is the current leader."
          )
        ]
      , p [ classes "fw8 mt4" ] [ text ( showRoundText model.round ) ]
      , if model.round == 2
        then div []
          [ button [ classes "ma2", onClick ( Vote True ) ] [ text "Agree" ]
          , button [ classes "ma2", onClick ( Vote False ) ] [ text "Disagree" ]
          ]
        else span [] []
      , if model.round == 4
        then div []
          [ button [ classes "ma2", onClick ( ChooseOutcome True ) ] [ text "Success!" ]
          , button [ classes "ma2", onClick ( ChooseOutcome True ) ] [ text "Fail!" ]
          ]
        else span [] []
      ]
      ( showTeamMembers model )
    )


showEndScreen : Model -> Html Msg
showEndScreen model =
  div []
    [ h1 [] [ text ( if hasPlayerWon model then "Congratulations! You have won!" else "You have lost." ) ]
    , showMissionStatus model
    , div [ classes "ma3" ] [ button [ onClick InitializeGame ] [ text "New Game" ] ]
    ]


showMissionStatus : Model -> Html Msg
showMissionStatus model =
  div 
  []
  ( List.indexedMap 
    ( \missionIndex outcome -> div [ classes "dib ma3 ba ph2" ] 
      [ h3 [ classes "f5" ] [ text ( "Mission " ++ String.fromInt ( missionIndex + 1 ) ) ]
      , p [ classes ( if outcome then "dark-green" else "red") ] 
          [ text ( if outcome then "Success" else "Fail" ) ]
      ]
    ) 
    model.missionOutcomes
  )

showTeamMembers : Model -> List (Html Msg)
showTeamMembers model =
  List.indexedMap 
  ( \index player -> div [ classes "dib ma4 ba ph2 w-10" ] 
    [ h3 [ classes "f4" ] [ text ( if index == 0 then model.name else player.name ) ]
    , if model.leader == index then span []
        [ p [ classes "fw5" ] [ i [ classes "fa fa-star gold" ] [], text " Leader" ] ]
      else p [ classes "white" ] [ text "-" ]
    , if player.selected
      then p [] [ i [ classes "fa fa-group green" ] [], text " selected" ]
      else
        if model.round == 1 && model.leader == 0 && getTeamSize model.players < getMaxTeamSize model.mission
        then button [ classes "db mv2 center", onClick ( SelectTeamMember index ) ] [ text "Select" ]
        else p [ classes "white" ] [ text "-" ]
    , if model.round == 3 || model.round == 4
      then p [] [ text ( if player.vote then "Vote: Agreed" else "Vote: Disagreed" ) ]
      else span [] []
    , if model.isTraitor && player.isTraitor
      then p [ classes "gray" ] [ text "hidden traitor" ]
      else p [ classes "white" ] [ text "-" ]
    , if model.round == 3
      then button [ classes "db mv2 center", onClick ( ChooseTraitor index ) ] [ text "Accuse" ]
      else span [] []
    ]
  )
  model.players

