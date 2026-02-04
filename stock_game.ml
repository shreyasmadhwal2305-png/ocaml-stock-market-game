(* RANDOM NUMBER SETUP *)
let () = Random.self_init ()

(* RISK LEVEL *)
type risk = | Low | Medium | High

(* MARKET EVENT *)
type market_event =
  | NoEvent
  | Boom of string        (* stock name *)
  | Crash of string
  | SectorBoom of risk
  | SectorCrash of risk

(* TRANSACTION TYPE *)
type transaction_type =
  | BuyTxn
  | SellTxn

type transaction = {
  day : int;
  stock : string;
  quantity : int;
  price : int;
  action : transaction_type;
}

(* STOCK RECORD *)
type stock = {
  name : string;
  price : int;
  risk : risk;
}

(* PLAYER HOLDING *)
type holding = {
  stock_name : string;
  quantity : int;
}

(* PLAYER STATE *)
type player = {
  cash : int;
  holdings : holding list;
  history : transaction list;
}

(* GAME STATE *)
type game_state = {
  day : int;
  stocks : stock list;
  player : player;
}

(* FIXED INITIAL STOCKS *)
let initial_stocks = [
  { name = "TCS";   price = 3500; risk = High };
  { name = "INFY";  price = 1600; risk = High };
  { name = "HDFC";  price = 1500; risk = Medium };
  { name = "ONGC";  price = 250;  risk = Medium };
  { name = "ITC";   price = 450;  risk = Low };
]

(* INITIAL PLAYER STATE *)
let initial_player = {
  cash = 10000;
  holdings = [];
  history = [];
}

(* INITIAL GAME STATE *)
let initial_game_state = {
  day = 1;
  stocks = initial_stocks;
  player = initial_player;
}

(* PRICE CHANGE PERCENTAGE BY RISK *)
let price_change_percent = function
  | Low -> 2
  | Medium -> 5
  | High -> 10

(* APPLY RANDOM PRICE CHANGE TO ONE STOCK *)
let update_stock_price stock =
  let max_change = price_change_percent stock.risk in
  let change_percent =
    (Random.int (2 * max_change + 1)) - max_change
  in
  let change_amount =
    stock.price * change_percent / 100
  in
  let new_price = stock.price + change_amount in
  let final_price =
    if new_price < 10 then 10 else new_price
  in
  { stock with price = final_price }

(* PICK RANDOM STOCK *)
let random_stock stocks =
  List.nth stocks (Random.int (List.length stocks))

(* RANDOM EVENT GENERATOR *)
let random_event stocks =
  let roll = Random.int 10 in
  match roll with
  | 0 ->
      let s = random_stock stocks in
      Boom s.name
  | 1 ->
      let s = random_stock stocks in
      Crash s.name
  | 2 -> SectorBoom High
  | 3 -> SectorCrash Medium
  | _ -> NoEvent

(* APPLY EVENT TO A STOCK *)
let apply_event stock event =
  match event with
  | NoEvent -> stock
  | Boom name when stock.name = name ->
      { stock with price = stock.price * 120 / 100 }
  | Crash name when stock.name = name ->
      { stock with price = max 10 (stock.price * 75 / 100) }
  | SectorBoom r when stock.risk = r ->
      { stock with price = stock.price * 110 / 100 }
  | SectorCrash r when stock.risk = r ->
      { stock with price = max 10 (stock.price * 85 / 100) }
  | _ -> stock

(* UPDATE MARKET WITH EVENTS *)
let update_market stocks =
  let event = random_event stocks in

  (match event with
   | NoEvent -> ()
   | Boom s -> Printf.printf "📰 NEWS: %s stock BOOMS!\n" s
   | Crash s -> Printf.printf "📰 NEWS: %s stock CRASHES!\n" s
   | SectorBoom r ->
       Printf.printf "📰 NEWS: %s sector rallies!\n"
         (match r with Low -> "Low" | Medium -> "Medium" | High -> "High")
   | SectorCrash r ->
       Printf.printf "📰 NEWS: %s sector crashes!\n"
         (match r with Low -> "Low" | Medium -> "Medium" | High -> "High")
  );

  stocks
  |> List.map update_stock_price
  |> List.map (fun s -> apply_event s event)

(* APPLY EVENT TO A STOCK *)
let apply_event stock event =
  match event with
  | NoEvent -> stock

  | Boom name when stock.name = name ->
      { stock with price = stock.price * 120 / 100 }

  | Crash name when stock.name = name ->
      { stock with price = stock.price * 75 / 100 }

  | SectorBoom r when stock.risk = r ->
      { stock with price = stock.price * 110 / 100 }

  | SectorCrash r when stock.risk = r ->
      { stock with price = stock.price * 85 / 100 }

  | _ -> stock

(* CALLS FOR RANDOMNESS IN DATA *)
let market1 = update_market initial_stocks;;
let market2 = update_market initial_stocks;;
let market3 = update_market initial_stocks;;

(* RANDOM EVENT GENERATOR *)
let random_event stocks =
  let roll = Random.int 10 in
  match roll with
  | 0 ->
      let s = List.nth stocks (Random.int (List.length stocks)) in
      Boom s.name
  | 1 ->
      let s = List.nth stocks (Random.int (List.length stocks)) in
      Crash s.name
  | 2 -> SectorBoom High
  | 3 -> SectorCrash Medium
  | _ -> NoEvent

(* DISPLAY THE CURRENT DAY *)
let print_day day =
  print_endline ("\n📅 Day " ^ string_of_int day)

(* DISPLAY MARKET PRICES *)
let print_market stocks =
  List.iter
    (fun s ->
      print_endline
        (s.name ^ " : ₹" ^ string_of_int s.price))
    stocks

(* ONE DAY OF THE GAME *)
let next_day state =
  let new_stocks = update_market state.stocks in
  {
    day = state.day + 1;
    stocks = new_stocks;
    player = state.player;
  }

(* ACTUAL GAME LOOP *)
let rec run_game state =
  if state.day > 30 then (
    print_endline "\n🏁 Game Over!";
    print_endline "\n📊 FINAL SUMMARY";
    state
  )
  else (
    print_day state.day;
    print_market state.stocks;

    let new_state = next_day state in
    run_game new_state
  )

(* PRINT DAY *)
let print_day day =
  print_endline ("\n📅 Day " ^ string_of_int day)

(* PRINT MARKET *)
let print_market stocks =
  print_endline "📈 Market prices:";
  List.iter
    (fun s ->
      Printf.printf "- %s : ₹%d (%s)\n"
        s.name
        s.price
        (match s.risk with
         | Low -> "Low"
         | Medium -> "Medium"
         | High -> "High"))
    stocks

(* PLAYER ACTION *)
type action = Buy | Sell | Skip

let ask_action () =
  print_endline "\nChoose action:";
  print_endline "1. Buy";
  print_endline "2. Sell";
  print_endline "3. Skip";
  print_string "> ";

  match read_line () with
  | "1" -> Buy
  | "2" -> Sell
  | "3" -> Skip
  | _ ->
      print_endline "❌ Invalid choice. Skipping.";
      Skip

(* DEFINES NEXT DAY LOGIC - PLAYER STATUS UNTOUCHED FOR NOW *)
let next_day state =
  {
    state with
    day = state.day + 1;
    stocks = update_market state.stocks;
  }

(* FIND A STOCK BY NAME *)
let find_stock name stocks =
  List.find_opt (fun s -> String.uppercase_ascii s.name = String.uppercase_ascii name) stocks

(* UPDATE HOLDINGS AFTER BUYING *)
let update_holdings stock_name qty holdings =
  let rec aux acc = function
    | [] ->
        List.rev ({ stock_name; quantity = qty } :: acc)
    | h :: t ->
        if h.stock_name = stock_name then
          List.rev_append acc ({ h with quantity = h.quantity + qty } :: t)
        else
          aux (h :: acc) t
  in
  aux [] holdings

(* PRINT CURRENT HOLDINGS *)
let print_holdings holdings =
  print_endline "📦 Your holdings:";
  if holdings = [] then
    print_endline "- None"
  else
    List.iter
      (fun h ->
        Printf.printf "- %s x%d\n" h.stock_name h.quantity)
      holdings

(* RECORD A TRANSACTION *)
let record_transaction state action stock qty price =
  let txn = {
    day = state.day;
    stock = stock;
    quantity = qty;
    price = price;
    action = action;
  } in
  txn :: state.player.history

(* BUY FUNCTION *)
let buy_stock state =
  print_endline "\nEnter stock name:";
  let stock_name = read_line () in

  match find_stock stock_name state.stocks with
  | None ->
      print_endline "❌ Stock not found.";
      state

  | Some stock ->
      print_endline "Enter quantity:";
      let qty =
        try read_int () with _ -> 0
      in

      if qty <= 0 then (
        print_endline "❌ Invalid quantity.";
        state
      )
      else
        let total_cost = stock.price * qty in
        if total_cost > state.player.cash then (
          print_endline "❌ Not enough cash.";
          state
        )
        else (
          print_endline "✅ Purchase successful.";

          {
            state with
            player =
              {
                cash = state.player.cash - total_cost;
                holdings =
                update_holdings stock.name qty state.player.holdings;
                history =
                record_transaction
                state BuyTxn stock.name qty stock.price;
              };
          }
        )

(* PRINT PLAYER CASH *)
let print_cash cash =
  Printf.printf "💰 Cash Available: ₹%d\n" cash

(* FIND A HOLDING BY STOCK NAME *)
let find_holding name holdings =
  List.find_opt
    (fun h ->
      String.uppercase_ascii h.stock_name =
      String.uppercase_ascii name)
    holdings

(* UPDATE HOLDINGS AFTER SELLING *)
let reduce_or_remove_holding stock_name qty holdings =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t ->
        if h.stock_name = stock_name then
          if h.quantity > qty then
            List.rev_append acc ({ h with quantity = h.quantity - qty } :: t)
          else
            List.rev_append acc t
        else
          aux (h :: acc) t
  in
  aux [] holdings

(* SELL STOCK *)
let sell_stock state =
  if state.player.holdings = [] then (
    print_endline "❌ You have no stocks to sell.";
    state
  ) else (
    print_holdings state.player.holdings;

    print_endline "Enter stock name to sell:";
    let stock_name = read_line () in

    match find_holding stock_name state.player.holdings with
    | None ->
        print_endline "❌ You do not own this stock.";
        state

    | Some holding ->
        print_endline "Enter quantity to sell:";
        let qty = try read_int () with _ -> 0 in

        if qty <= 0 || qty > holding.quantity then (
          print_endline "❌ Invalid quantity.";
          state
        ) else
          match find_stock stock_name state.stocks with
          | None ->
              print_endline "❌ Stock not found in market.";
              state
          | Some stock ->
              let earnings = qty * stock.price in
              Printf.printf "✅ Sold for ₹%d\n" earnings;

              {
                state with
                player =
                  {
                      cash = state.player.cash + earnings;
                      holdings =
                        reduce_or_remove_holding
                          stock_name qty state.player.holdings;
                      history =
                        record_transaction
                          state SellTxn stock.name qty stock.price;
                  };
              }
            )

(* GET STOCK PRICE BY NAME *)
let stock_price name stocks =
  match find_stock name stocks with
  | Some stock -> stock.price
  | None -> 0

(* VALUE A SINGLE HOLDING *)
let holding_value holding stocks =
  let price = stock_price holding.stock_name stocks in
  holding.quantity * price

(* TOTAL NET WORTH *)
let net_worth state =
  let holdings_value =
    List.fold_left
      (fun acc h -> acc + holding_value h state.stocks)
      0
      state.player.holdings
  in
  state.player.cash + holdings_value

(* PRINT NET WORTH *)
let print_net_worth amount =
  Printf.printf "💼 Net Worth: ₹%d\n" amount

(* PRINT TRANSACTION HISTORY *)
let print_history history =
  print_endline "\n📜 Transaction History:";
  if history = [] then
    print_endline "No trades made."
  else
    List.iter
      (fun t ->
        let action =
          match t.action with
          | BuyTxn -> "BUY"
          | SellTxn -> "SELL"
        in
        Printf.printf
          "Day %d | %s | %s | Qty: %d | Price: ₹%d\n"
          t.day action t.stock t.quantity t.price
      )
      (List.rev history)

(* CHEAPEST STOCK PRICE *)
let cheapest_stock_price stocks =
  stocks
  |> List.map (fun s -> s.price)
  |> List.fold_left min max_int

(* BANKRUPTCY CHECK FUNCTION *)
let is_bankrupt state =
  state.player.cash < cheapest_stock_price state.stocks
  && state.player.holdings = []

(* INTERACTIVE GAME LOOP *)
let rec run_game state =
  if is_bankrupt state then (
    print_endline "\n💀 BANKRUPTCY!";
    print_cash state.player.cash;
    print_holdings state.player.holdings;
    print_net_worth (net_worth state);
    print_history state.player.history;
    state
  )
  else if state.day > 30 then (
    print_endline "\n🏁 Game Over!";
    print_cash state.player.cash;
    print_holdings state.player.holdings;
    print_net_worth (net_worth state);
    print_history state.player.history;
    state
  )
  else (
    print_day state.day;
    print_cash state.player.cash;
    print_market state.stocks;
    print_net_worth (net_worth state);

    let state_after_action =
      match ask_action () with
      | Buy ->
          let updated = buy_stock state in
          print_holdings updated.player.holdings;
          updated

      | Sell ->
          let updated = sell_stock state in
          print_holdings updated.player.holdings;
          updated

      | Skip ->
          print_endline "⏭ Skipping day.";
          state
    in

    let next_state = next_day state_after_action in
    run_game next_state
  )

(* in utop - #use "stock_game.ml";;
run_game initial_game_state;; to run this code *)