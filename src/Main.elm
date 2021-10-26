module Main exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Random
import Set
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
  { id: Int
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
  ( classNames
    |> String.split " "
    |> List.map ( \className -> ( className, True ) )
  )


defaultPlayer : Player
defaultPlayer =
  { id = 0, name = "Foobar", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }


initPlayers : String -> List Player
initPlayers playerName =
  [ { id = 0, name = playerName, isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { id = 1, name = "Mary", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { id = 2, name = "Robert", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { id = 3, name = "Jennifer", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { id = 4, name = "David", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { id = 5, name = "Lisa", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  , { id = 6, name = "George", isTraitor = False, selected = False, vote = False, failedMissions = 0, accuses = -1, suspicions = 0 }
  ]


showRoundText : Int -> Int -> String
showRoundText mission round =
  case round of
    1 -> "Who will you take with you on your mission?"
    2 -> ("Vote on the selected team. " ++ if mission == 4 then "This mission 4 requires 2 fails to fail." else "" )
    3 -> "Choose a traitor suspect. All players will remember this."
    4 -> ("Choose your mission outcome. " ++ if mission == 4 then "This mission 4 requires 2 fails to fail." else "" )
    _ -> "Something went wrong, game ended."


getMaxTeamSize : Int -> Int
getMaxTeamSize mission =
  case mission of
    1 -> 2
    2 -> 3
    3 -> 3
    4 -> 4
    5 -> 4
    _ -> 3


getTeamSize : List Player -> Int
getTeamSize players = List.length ( List.filter .selected players )


getPlayer : Int -> List Player -> Player
getPlayer playerId players =
  players
  |> List.filter ( \player -> player.id == playerId )
  |> List.head
  |> Maybe.withDefault defaultPlayer


aiAssignTeam : Int -> Int -> List Player -> List Player
aiAssignTeam leader mission players =
  let
    isTraitor : Bool
    isTraitor =
      players
      |> List.map ( \player -> if player.id == leader then player.isTraitor else False )
      |> List.member True
    
    otherTraitorId : Int
    otherTraitorId =
      players
      |> List.filter ( \player -> player.id /= leader && player.isTraitor )
      |> List.map ( \player -> player.id )
      |> List.head
      |> Maybe.withDefault 0
    
    team : List Int
    team =
      players
      |> List.sortBy
        ( \player -> 
          if player.id == leader 
          then -1 
          else
            if isTraitor && mission == 4 && player.id == otherTraitorId
            then -1
            else player.suspicions
        )
      |> List.take ( getMaxTeamSize mission )
      |> List.map .id
   
   in
    List.map 
    ( \player -> { player | selected = List.member player.id team } )
    players


aiVoteTeam : Int -> Int -> List Player -> List Bool -> Bool
aiVoteTeam leader playerId players missionOutcomes =
  let
    isTraitor : Bool
    isTraitor =
      selectedPlayers
      |> List.map ( \player -> if player.id == leader then player.isTraitor else False )
      |> List.member True
    
    selectedPlayers : List Player
    selectedPlayers = List.filter .selected players
    
    hasFailedMissions : Bool
    hasFailedMissions = List.any ( \player -> player.failedMissions > 0 ) selectedPlayers
    
    highestTeamSuspicion : Float
    highestTeamSuspicion = 
      selectedPlayers
      |> List.sortBy .suspicions
      |> List.map .suspicions
      |> List.reverse
      |> List.head
      |> Maybe.withDefault 0
      |> toFloat
    
    groupAverageSuspicions : Float
    groupAverageSuspicions = 
      ( toFloat << List.sum << List.map .suspicions ) players / ( toFloat << List.length ) players
  
  in
    if not isTraitor || List.length ( List.filter ( \outcome -> outcome ) missionOutcomes ) < 2
    then True
    else
      if isTraitor && List.length ( List.filter ( \player -> player.selected && player.isTraitor ) players ) < 2
      then False
      else
        if
          playerId == leader 
          || not hasFailedMissions 
          || ( highestTeamSuspicion <= groupAverageSuspicions )
        then True
        else False


aiChoosePossibleTraitor : List Player -> List Int
aiChoosePossibleTraitor players =
    players
    |> List.append players
    |> List.append players
    |> List.filter ( \player -> player.selected )
    |> List.reverse
    |> List.map2 ( \index player -> ( index, player.id ) ) ( List.range 0 24 )
    |> List.filter ( \( index, id ) -> index /= id )
    |> List.map ( \( index, id ) -> id )
    |> List.take ( List.length players - 1 )


updateSuspicions : List Player -> List Int -> List Player
updateSuspicions players suspicionIds =
  List.map
  ( \player -> 
    { player
    | accuses = List.sum <| List.indexedMap ( \id accusedId -> if id == player.id then accusedId else 0 ) suspicionIds
    , suspicions = player.suspicions + List.length ( List.filter ( \id -> id == player.id ) suspicionIds )
    , selected = False
    } 
  )
  players


aiChooseMissionOutcome : Int -> Model -> Bool
aiChooseMissionOutcome playerId model =
  let
    selectedPlayers : List Player
    selectedPlayers = List.filter .selected model.players
    
    isTraitor : Bool
    isTraitor =
      selectedPlayers
      |> List.map ( \player -> if player.id == playerId then player.isTraitor else False )
      |> List.member True
    
    numberOfSuspicions : Float
    numberOfSuspicions = 
      ( toFloat << List.sum << List.indexedMap ( \id player -> if id == playerId then player.suspicions else 0 ) ) selectedPlayers
    
    averageGroupSuspicions : Float
    averageGroupSuspicions = 
      ( toFloat << List.sum << List.map .suspicions ) model.players / ( toFloat << List.length ) model.players
  
  in
    if isTraitor && model.mission == 1 && model.isAggressiveRound
    then False
    else
      if not isTraitor || model.mission == 1
      then True
      else
        if model.mission == 5
        then False
        else
          if model.mission > 3 && ( List.length << List.filter ( \outcome -> outcome ) ) model.missionOutcomes < 2
          then False
          else
            if ( numberOfSuspicions < averageGroupSuspicions )
            then False
            else True


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
    , suspicions =
        if not player.selected 
        then player.suspicions
        else
          if not success then  player.suspicions + 2 else if player.suspicions - 2 < 0 then 0 else player.suspicions - 2
    , selected = if success then False else player.selected
    }
  )
  players


assignOutcomes : Model -> List Bool -> Model
assignOutcomes model outcomes =
  let
    success : Bool
    success = 
      ( List.length << List.filter ( \currentOutcome -> currentOutcome == False ) ) outcomes < ( if model.mission == 4 then 2 else 1 )
    
    nextLeader : Int
    nextLeader = if model.leader + 1 >= List.length model.players then 0 else model.leader + 1
  
  in
    { model 
      | players =
          if not success || nextLeader == 0
          then updatePlayerOutcomes success model.players
          else aiAssignTeam nextLeader ( model.mission + 1 ) ( updatePlayerOutcomes success model.players )
      , mission = model.mission + 1
      , leader = if success then nextLeader else model.leader
      , round = if not success then 3 else if nextLeader == 0 then 1 else 2
      , missionOutcomes = model.missionOutcomes ++ [ success ]
    }


hasGameEnded : Model -> Bool
hasGameEnded model =
  ( List.length << List.filter ( \outcome -> outcome == True ) ) model.missionOutcomes == 3
  || ( List.length << List.filter ( \outcome -> outcome == False ) ) model.missionOutcomes == 3


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
  , isAggressiveRound: Bool
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
    False
    ( initPlayers "Player" )
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
  | AssignAggression Int
  | AssignRoles (List Int)
  | AssignLeader Int
  | SelectTeamMember Int
  | Vote Bool
  | ChooseSuspect Int
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
          ( \playerId player ->
            { player | name = if playerId == 0 then model.name else player.name }
          )
          model.players
        }
      , Cmd.none
      )
    
    InitializeGame ->
      ( { model
        | players = initPlayers model.name
        , mission = 1
        , round = 1
        , voteTrack = 1
        , missionOutcomes = []
        }
      , Random.generate AssignAggression (Random.int 0 1)
      )
    
    AssignAggression isAggressive ->
      ( { model
        | isAggressiveRound = if isAggressive == 1 then True else False
        }
      , Random.generate AssignRoles (Random.list 3 (Random.int 0 6))
      )

    AssignRoles [] ->
      ( model
      , Cmd.none
      )

    AssignRoles traitorIds ->
        ( 
          { model
          | isTraitor = List.member 0 traitorIds
          , players =
            List.indexedMap
            ( \id player ->
              { player | isTraitor = List.member id traitorIds }
            )
            model.players
          , mission = 1
          }
        , if Set.size ( Set.fromList traitorIds ) < 3
          then Random.generate AssignRoles (Random.list 3 (Random.int 0 6))
          else Random.generate AssignLeader (Random.int 0 4)
        )
    
    AssignLeader leader ->
      ( 
        if leader == 0
        then { model | leader = leader, round = 1 }
        else { model | leader = leader, players = aiAssignTeam leader model.mission model.players, round = 2 }
        , Cmd.none
      )
    
    SelectTeamMember playerId ->
      let
        finishedSelection : Bool
        finishedSelection = getTeamSize model.players == ( getMaxTeamSize model.mission ) - 1
        
        isPlayerInTeam : Bool
        isPlayerInTeam = List.any ( \player -> player.id == 0 ) ( List.filter .selected model.players )
      
      in
        (
          { model 
          | players = 
            List.map 
            ( \player -> if player.id == playerId then { player | selected = True } else player ) 
            model.players
          , round = if finishedSelection then 2 else 1
          }
        , Cmd.none
        )
    
    Vote vote -> 
      let
        votes : List Bool
        votes = vote ::
          ( model.players
            |> List.filter ( \player -> player.id > 0 )
            |> List.map ( \player -> aiVoteTeam model.leader player.id model.players model.missionOutcomes )
          )
        
        success : Bool
        success = 
          toFloat ( ( List.length << List.filter ( \currentVote -> currentVote ) ) votes )
          >= ( toFloat ( List.length votes ) / 2 )
        
        newPlayers : List Player
        newPlayers =
          List.map2
          ( \playerVote player ->
            { player
            | vote = playerVote
            , suspicions = if playerVote then player.suspicions else player.suspicions + 1
            }
          )
          votes
          model.players
        
        newModel : Model
        newModel =
          { model
          | players = newPlayers
          , teamAllowed = success
          , voteTrack = if not success then model.voteTrack + 1 else 1
          }
        
        isPlayerInTeam : Bool
        isPlayerInTeam = 
          model.players
          |> List.filter .selected
          |> List.map .id
          |> List.member 0
      
      in
        ( 
          if isPlayerInTeam
          then
            { newModel | round = if success then 4 else 3 }
          else
            assignOutcomes
            newModel
            ( model.players
              |> List.filter ( \player -> player.selected )
              |> List.map ( \player -> aiChooseMissionOutcome player.id model )
            )
        , Cmd.none
        )
    
    ChooseSuspect suspicionId -> 
      let
        suspicionIds : List Int
        suspicionIds = suspicionId :: ( aiChoosePossibleTraitor model.players )
        --suspicionIds = suspicionId ::
        --  ( model.players
        --    |> List.filter ( \player -> player.id > 0 )
        --    |> List.map ( \player -> aiChoosePossibleTraitor player.id model.players )
        --  )
        
        nextLeader : Int
        nextLeader = if model.leader + 1 >= List.length model.players then 0 else model.leader + 1
        
        nextMission : Int
        nextMission = if model.voteTrack == 5 then model.mission + 1 else model.mission
      
      in
        ( { model 
          | players =
              if nextLeader == 0
              then updateSuspicions model.players suspicionIds
              else aiAssignTeam nextLeader nextMission ( updateSuspicions model.players suspicionIds )
          , mission = nextMission
          , leader = nextLeader
          , round = if nextLeader == 0 then 1 else 2
          , voteTrack = if model.voteTrack == 5 then 1 else model.voteTrack
          , missionOutcomes = if model.voteTrack == 5 then model.missionOutcomes ++ [ False ] else model.missionOutcomes
          }
        , Cmd.none
        )
    
    ChooseOutcome outcome -> 
      ( assignOutcomes
        model
        ( outcome ::
          ( model.players
            |> List.filter ( \player -> player.id > 0 && player.selected )
            |> List.map ( \player -> aiChooseMissionOutcome player.id model )
          )
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
    [ h1 [ classes "f2 fw3 mb0 orange" ] [ text "Welcome to Avalon" ]
    , p [ classes "mt0 mb4 black-80" ] [ text "a game of trust and betrayal" ]
    , label [ classes "mr3 dark-blue" ] [ text "Enter your name:" ]
    , input [ classes" bg-white ba b--black black pa2 bg-washed-yellow", placeholder "Player", value model.name, onInput EnterName ] []
    , div [ classes "ma3" ] [ button [ classes "bg-white ba b--black black pv2 ph3 bw1 br3 bg-lightest-blue pointer", onClick InitializeGame ] [ text "Start" ] ]
    ]


showTeamScreen : Model -> Html Msg
showTeamScreen model =
  div []
    ( 
      List.append
      [ showMissionStatus model
      , if model.isTraitor
        then div []
          [ p [ classes "light-red fw7 mb0" ] [ text "You are a traitor. You can see the other traitors." ]
          , p [ classes "gray mt0" ] [ text "Fail the majority of missions to win this game. Try to not to be suspected as traitor." ]
          ]
        else
          div [] 
            [ p [ classes "green fw7 mb0" ] [ text "You are loyal." ]
            , p [ classes "gray mt0" ] [ text "Succeed the majority of missions to win this game. Beware of 2 hidden traitors." ]
            ]
      , p []
        ( List.append
          [ text "Voting round: " ]
          ( List.map 
            ( \round -> 
              span [] 
                [ i [ classes ( "fa fa-" ++ if round > model.voteTrack then "circle black-20" else if round < model.voteTrack then "times red" else "circle yellow" ) ] []
                , span [] [ text " " ] 
                ]
            ) 
            ( List.range 1 5 )
          )
        )
      , p [ classes "fw8 mt4" ] [ text ( showRoundText model.mission model.round ) ]
      , if model.round == 2
        then div []
          [ button [ classes "ma2 bg-white ba b--black pv2 ph3 bw1 br3 bg-lightest-blue pointer", style "font-size" "40px", onClick ( Vote True ) ] 
              [ i [ classes "fa fa-thumbs-up green" ] [] ]
          , button [ classes "ma2 bg-white ba b--black pv2 ph3 bw1 br3 bg-lightest-blue pointer", style "font-size" "40px", onClick ( Vote False ) ] 
              [ i [ classes "fa fa-thumbs-down red" ] [] ]
          ]
        else span [] []
      , if model.round == 4
        then div []
          [ button [ classes "ma2 bg-white ba b--black pv2 ph3 bw1 br3 bg-lightest-blue pointer", style "font-size" "40px", onClick ( ChooseOutcome True ) ] 
              [ i [ classes "fa fa-check green" ] [] ]
          , button [ classes "ma2 bg-white ba b--black pv2 ph3 bw1 br3 bg-lightest-blue pointer", style "font-size" "40px", onClick ( ChooseOutcome False ) ] 
              [ i [ classes "fa fa-times red" ] [] ]
          ]
        else span [] []
      ]
      [ div [ classes "center", style "max-width" "800px" ] ( showTeamMembers model ) ]
    )


showEndScreen : Model -> Html Msg
showEndScreen model =
  div []
    [ h1 [] [ text ( if hasPlayerWon model then "Congratulations! You have won!" else "You have lost." ) ]
    , showMissionStatus model
    , div [ classes "ma3" ] 
        [ button 
          [ classes "bg-white ba b--black black pv2 ph3 bw1 br3 bg-lightest-blue pointer", onClick InitializeGame ] 
          [ text "New Game" ] 
        ]
    ]


showMissionStatus : Model -> Html Msg
showMissionStatus model =
  div 
  []
  ( List.indexedMap 
    ( \missionIndex outcome -> 
      div 
        [ classes 
            ( "dib ma3 ba ph2 bg-washed-" ++ 
              if missionIndex < model.mission
              then
                if model.mission - 1 == missionIndex 
                then "yellow shadow-1" 
                else 
                  if outcome 
                  then "green" 
                  else "red"
              else ""
            ) 
        ] 
        [ h3 [ classes "f5" ] [ text ( "Mission " ++ String.fromInt ( missionIndex + 1 ) ) ]
        , p [ classes ( if outcome then "dark-green" else "red") ] 
            [ if missionIndex < model.mission - 1
              then
                if outcome
                then i [ classes "fa fa-check green", style "font-size" "40px" ] []
                else i [ classes "fa fa-times red", style "font-size" "40px" ] []
              else span [ classes "white", style "font-size" "40px" ] [ text "-" ]
            ]
        ]
    ) 
    ( List.append model.missionOutcomes ( List.repeat ( 5 - List.length model.missionOutcomes ) False ) )
  )
  
getMemberColor : Player -> Int -> String
getMemberColor player leaderId =
  if player.id == leaderId then "bg-light-yellow"
  else if player.selected then "bg-light-yellow"
  else "bg-washed-green"

showTeamMembers : Model -> List (Html Msg)
showTeamMembers model =
  List.indexedMap 
  ( \id player -> 
    div 
    [ classes ( String.concat [ "relative dib ma4 ba ph2 w-10 ", ( getMemberColor player model.leader ) ] ), style "min-width" "120px" ] 
    [ h3 [ classes "f4 black-80" ] [ text ( if id == 0 then model.name else player.name ) ]
    , if model.leader == id then span []
        [ p [ classes "fw5" ] [ text " Leader" ] ]
      else p [ classes "white" ] [ text "-" ]
    , if player.selected
      then p [ style "font-size" "40px", classes "mv1" ]
        [ if model.leader == id then i [ classes "fa fa-flag gold" ] [] else span [] []
        , span [] [ text " " ]
        , i [ classes "fa fa-check-circle-o green", style "font-size" "40px" ] []
        ]
      else
        if model.round == 1 && model.leader == 0 && getTeamSize model.players < getMaxTeamSize model.mission
        then button [ classes "db mv2 center bg-white ba b--black pv2 ph3 bw1 br3 bg-lightest-blue pointer", onClick ( SelectTeamMember id ) ] [ i [ classes "fa fa-check-circle-o green", style "font-size" "40px" ] [] ]
        else p [ style "font-size" "40px", classes "mv1 white" ] [ text "-" ]
    , if model.round == 3 || model.round == 4
      then
        if player.vote
        then p [ style "font-size" "21px" ] [ text "Vote: ", i [ classes "fa fa-thumbs-up green" ] [] ]
        else p [ style "font-size" "21px" ] [ text "Vote: ", i [ classes "fa fa-thumbs-down red" ] [] ]
      else span [] []
    , if model.isTraitor && player.isTraitor
      then p [ classes "gray" ] [ text "hidden traitor" ]
      else p [ classes "white" ] [ text "-" ]
    , if model.round == 3
      then button [ classes "db mv2 center bg-white ba b--black black pv2 ph3 bw1 br3 bg-lightest-blue pointer", onClick ( ChooseSuspect id ) ] [ text "Accuse" ]
      else span [] []
    , if ( model.round == 1 || model.round == 2 ) && player.accuses /= -1
      then div []
        [ div [] [ text "Accuses:" ]
        , div []
          [
            text
            ( String.concat
              ( List.map 
                .name
                ( List.filter ( \accusedPlayer -> player.accuses == accusedPlayer.id ) model.players )
              )
            )
          ]
        ]
      else span [] []
    --, p [ classes "gray" ] [ text ( "S: " ++ String.fromInt player.suspicions ) ]
    ]
  )
  model.players

