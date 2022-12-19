#r "nuget: Unquote"
open Swensen.Unquote

//Pruned BFS with memoization
//Pruning heuristics (taken from hyper-neutrino YT)
//1. spend rates: if you can only use x resY/minute, it does not make sense to have more than x robotY
//2. equivalent resource pools: if there are 10 minutes left and you have an ore spend rate of 4, all states with ore resources >= 4 * 10 = 40 are functionally equivalent
// Geode pools are not equivalent because we're trying to max that :)

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type ResourceType =
    | Ore
    | Clay
    | Obsidian
    | Geode

type RobotType = RobotType of ResourceType
type Cost = Cost of int

type Blueprint =
    { Id: int
      Costs: Map<RobotType, (Cost * ResourceType) list> }

let parse (blueprint: string) : Blueprint =
    let re =
        System.Text.RegularExpressions.Regex(
            "Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\."
        )

    let m = re.Match(blueprint)
    let id = m.Groups[1].Value |> int
    let oreRobotOreCost = m.Groups[2].Value |> int
    let clayRobotOreCost = m.Groups[3].Value |> int
    let obsRobotOreCost = m.Groups[4].Value |> int
    let obsRobotClayCost = m.Groups[5].Value |> int
    let geoRobotOreCost = m.Groups[6].Value |> int
    let geoRobotObsCost = m.Groups[7].Value |> int

    { Id = id
      Costs =
        [ (RobotType Ore, [ (Cost oreRobotOreCost, ResourceType.Ore) ])
          (RobotType Clay, [ (Cost clayRobotOreCost, ResourceType.Ore) ])
          (RobotType Obsidian,
           [ (Cost obsRobotOreCost, ResourceType.Ore)
             (Cost obsRobotClayCost, ResourceType.Clay) ])
          (RobotType Geode,
           [ (Cost geoRobotOreCost, ResourceType.Ore)
             (Cost geoRobotObsCost, ResourceType.Obsidian) ]) ]
        |> Map.ofList }

type State =
    { Robots: Map<RobotType, int>
      AvailableResources: Map<ResourceType, int> }

let init =
    { Robots =
        [ (RobotType Ore, 1)
          (RobotType Clay, 0)
          (RobotType Obsidian, 0)
          (RobotType Geode, 0) ]
        |> Map.ofList
      AvailableResources =
        [ (ResourceType.Ore, 0)
          (ResourceType.Clay, 0)
          (ResourceType.Obsidian, 0)
          (ResourceType.Geode, 0) ]
        |> Map.ofList }

let collectResources state =
    state.Robots
    |> Map.toList
    |> List.map (fun (RobotType rt, x) -> rt, x)

let addResource state (r, amt) =
    let curr = state.AvailableResources |> Map.find r
    let added = state.AvailableResources |> Map.add r (amt + curr)
    { state with AvailableResources = added }

let addResources collected state =
    collected |> List.fold addResource state

let available (state: State) (Cost c, r: ResourceType) =
    let a = state.AvailableResources |> Map.find r
    a >= c

let allAvailable costs state = costs |> List.forall (available state)

type FactoryAction =
    | MakeRobot of RobotType
    | DoNothing


type Constraints = { SpendRates: Map<ResourceType, Cost> }

let useful (constraints: Constraints) (state: State) (RobotType rt) =
    match constraints.SpendRates |> Map.tryFind rt with
    | Some (Cost spendRate) ->
        let currentRate = state.Robots |> Map.find (RobotType rt)
        currentRate < spendRate
    | None -> true //Geode robot

let possibleActions (constraints: Constraints) (blueprint: Blueprint) state =
    blueprint.Costs
    |> Map.toList
    |> List.filter (fun (rt, costs) -> allAvailable costs state)
    |> List.filter (fun (rt, costs) -> useful constraints state rt)
    |> List.map (fst >> MakeRobot)
    |> List.append [ DoNothing ] //Not building a robot is always a valid action

let consume state (Cost c, r) =
    let curr = state.AvailableResources |> Map.find r
    { state with AvailableResources = state.AvailableResources |> Map.add r (curr - c) }

let addRobot rt state =
    let curr = state.Robots |> Map.find rt
    let next = state.Robots |> Map.add rt (curr + 1)
    { state with Robots = next }

let makeRobot (blueprint: Blueprint) (state: State) (RobotType r) =
    let c = blueprint.Costs |> Map.find (RobotType r)
    let consumed = c |> List.fold (fun s t -> consume s t) state
    let added = consumed |> addRobot (RobotType r)
    added

let perform blueprint state action =
    match action with
    | DoNothing -> state
    | MakeRobot r -> makeRobot blueprint state r

let tick constraints blueprint state =
    state
    |> possibleActions constraints blueprint
    |> List.map (perform blueprint state)
    |> List.map (addResources (collectResources state))

///Calculates what the maximum spend is for each resource in a single tick.
let spendRates (blueprint: Blueprint) =
    blueprint.Costs
    |> Map.toList
    |> List.collect snd
    |> List.groupBy snd
    |> List.map (fun (rt, costs) -> (rt, costs |> List.map fst |> List.max))
    |> Map.ofList

let buildConstraints blueprint = { SpendRates = spendRates blueprint }

let pruneSpendRate state (rt, Cost c) =
    let robots = state.Robots |> Map.find (RobotType rt)

    if robots <= c then
        state
    else
        //printfn "PRUNING %A %A in %A" rt c state
        failwith "BUG: we should not be building the robot instead of releasing it and having resource consumption!"
        { state with Robots = state.Robots |> Map.add (RobotType rt) c }

let pruneResource timeleft state rt (Cost c) =
    let capped =
        state.AvailableResources
        |> Map.map (fun avrt avamt ->
            if avrt <> rt then
                avamt
            else
                min avamt (timeleft * c))

    { state with AvailableResources = capped }

let prune timeleft (constraints: Constraints) state =
    constraints.SpendRates
    |> Map.fold (pruneResource timeleft) state

let steps () max blueprint state =
    let memo = System.Collections.Generic.Dictionary<State, Set<State>>()
    let constraints = buildConstraints blueprint

    let rec memoSteps minute state =
        //printfn "Minute %d" minute

        if minute = max then
            Set.singleton state
        else
            match memo.TryGetValue state with
            | true, m ->
                //printfn "CACHE HIT for %A, i.e: %A" state m
                m
            | false, _ ->
                //printfn "CACHE MISS for %A" state
                let next = tick constraints blueprint state
                //printfn "Next possible states: %A" next
                let pruned =
                    next
                    |> Set.ofList
                    |> Set.map (prune (max - minute) constraints)
                //printfn "Pruned next states: %A" pruned

                let results =
                    pruned
                    |> Set.map (memoSteps (minute + 1))
                    |> Set.unionMany

                memo.TryAdd(state, results) |> ignore
                results

    memoSteps 0 state

let winningFuture states =
    states
    |> Set.toList
    |> List.sortByDescending (fun s ->
        s.AvailableResources
        |> Map.find ResourceType.Geode)
    |> Seq.head

let countGeodes (state: State) =
    state.AvailableResources |> Map.find Geode

let blueprints = example |> List.map parse
#time
let states = steps () 24 blueprints[1] init
let w = winningFuture states
countGeodes w

let qualityLevels =
    blueprints
    |> List.map (fun blueprint ->
        printfn "Running simulations for blueprint %d" blueprint.Id
        let states = steps () 24 blueprint init

        let bestFuture = winningFuture states

        let nbGeodes = countGeodes bestFuture
        let quality = blueprint.Id * nbGeodes
        quality)

qualityLevels |> List.sum

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
