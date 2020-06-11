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
    team : List Int
    team =
      players
      |> List.sortBy ( \player -> if player.id == leader then -1 else player.suspicions )
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
    
    teamAverageSuspicions : Float
    teamAverageSuspicions = 
      ( toFloat << List.sum << List.map .suspicions ) selectedPlayers / ( toFloat << List.length ) selectedPlayers
    
    groupAverageSuspicions : Float
    groupAverageSuspicions = 
      ( toFloat << List.sum << List.map .suspicions ) players / ( toFloat << List.length ) players
  
  in
    if not isTraitor || List.length ( List.filter ( \outcome -> outcome ) missionOutcomes ) < 2
    then
      playerId == leader 
      || not hasFailedMissions 
      || ( teamAverageSuspicions <= groupAverageSuspicions )
    else
      False

aiChoosePossibleTraitor : Int -> List Player -> Int
aiChoosePossibleTraitor playerId players =
  players
  |> List.filter ( \player -> player.id /= playerId )
  |> List.sortBy .failedMissions
  |> List.map .id
  |> List.reverse
  |> List.head
  |> Maybe.withDefault 0


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
    if not isTraitor || model.mission == 1
    then True
    else
      if model.mission == 5
      then False
      else
        if model.mission > 2 && ( List.length << List.filter ( \outcome -> outcome ) ) model.missionOutcomes == 2
        then False
        else
          (model.mission == 4) || ( numberOfSuspicions < averageGroupSuspicions )


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
          if success then player.suspicions - 2 else player.suspicions + 2
    , selected = False
    }
  )
  players


assignOutcomes : Model -> List Bool -> Model
assignOutcomes model outcomes =
  let
    success : Bool
    success = ( List.length << List.filter ( \currentOutcome -> currentOutcome == False ) ) outcomes == 0
    
    nextLeader : Int
    nextLeader = if model.leader + 1 >= List.length model.players then 0 else model.leader + 1
  
  in
    { model 
      | players =
          if nextLeader == 0
          then updatePlayerOutcomes success model.players
          else aiAssignTeam nextLeader ( model.mission + 1 ) ( updatePlayerOutcomes success model.players )
      , mission = model.mission + 1
      , leader = nextLeader
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
      , Random.generate AssignRoles (Random.list 2 (Random.int 0 4))
      )

    AssignRoles [] ->
      ( model
      , Cmd.none
      )

    AssignRoles ( firstTraitor :: otherTraitor ) ->
      let
        traitorIds : List Int
        traitorIds = 
          firstTraitor :: if [ firstTraitor ] == otherTraitor then [ firstTraitor + 1 ] else otherTraitor
      
      in
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
        , Random.generate AssignLeader (Random.int 0 4)
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
        suspicionIds = suspicionId ::
          ( model.players
            |> List.filter ( \player -> player.id > 0 )
            |> List.map ( \player -> aiChoosePossibleTraitor player.id model.players )
          )
        
        nextLeader : Int
        nextLeader = if model.leader + 1 >= List.length model.players then 0 else model.leader + 1
        
        nextMission : Int
        nextMission = if model.voteTrack > 5 then model.mission + 1 else model.mission
      
      in
        ( { model 
          | players =
              if nextLeader == 0
              then updateSuspicions model.players suspicionIds
              else aiAssignTeam nextLeader nextMission ( updateSuspicions model.players suspicionIds )
          , mission = nextMission
          , leader = nextLeader
          , round = if nextLeader == 0 then 1 else 2
          , voteTrack = if model.voteTrack > 5 then 1 else model.voteTrack
          , missionOutcomes = if model.voteTrack > 5 then model.missionOutcomes ++ [ False ] else model.missionOutcomes
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
      , p [] [ text ( "The next mission will fail if the team can't agree on a team in 5 vote rounds." ) ]
      , p [] [ text ( "Vote round " ++ String.fromInt model.voteTrack ++ " out of 5." ) ]
      , p [ classes "cb" ] 
        [ text
          ( 
            (
              String.concat 
              ( List.map .name ( List.filter ( \player -> player.id == model.leader ) model.players ) )
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
          , button [ classes "ma2", onClick ( ChooseOutcome False ) ] [ text "Fail!" ]
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
  ( \id player -> div [ classes "dib ma4 ba ph2 w-10" ] 
    [ h3 [ classes "f4" ] [ text ( if id == 0 then model.name else player.name ) ]
    , if model.leader == id then span []
        [ p [ classes "fw5" ] [ i [ classes "fa fa-star gold" ] [], text " Leader" ] ]
      else p [ classes "white" ] [ text "-" ]
    , if player.selected
      then p [] [ i [ classes "fa fa-group green" ] [], text " selected" ]
      else
        if model.round == 1 && model.leader == 0 && getTeamSize model.players < getMaxTeamSize model.mission
        then button [ classes "db mv2 center", onClick ( SelectTeamMember id ) ] [ text "Select" ]
        else p [ classes "white" ] [ text "-" ]
    , if model.round == 3 || model.round == 4
      then p [] [ text ( if player.vote then "Vote: Agreed" else "Vote: Disagreed" ) ]
      else span [] []
    , if model.isTraitor && player.isTraitor
      then p [ classes "gray" ] [ text "hidden traitor" ]
      else p [ classes "white" ] [ text "-" ]
    , if model.round == 3
      then button [ classes "db mv2 center", onClick ( ChooseSuspect id ) ] [ text "Accuse" ]
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
    ]
  )
  model.players

