Set Warnings "-notation-overridden".

Require Import Coq.Strings.String.
Require Import Coq.Reals.Reals.
Require Import Coq.ZArith.Int.

Generalizable All Variables.
Set Primitive Projections.
Set Universe Polymorphism.
Unset Transparent Obligations.

Open Scope R_scope.

(* - represent the greeks
   - define composite strategies (condors, spreads)
   - equivalent positions (S = C - P)
     https://www.investopedia.com/articles/optioninvestor/09/equivalent-positions.asp
   - pricing models (Black-Scholes, binomial)
 *)

Inductive Kind := Call | Put.
Inductive Side := Long | Short.

Definition Symbol := string.
Definition Amount := R.

Inductive Month :=
  Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec.

Definition Day  := nat.
Definition Year := nat.
Definition Date := (Day * Month * Year)%type.

Inductive Equity (symbol : Symbol) :=
  | Stock
  | ETF.

(* A contract is independent of the price of the underlying. *)
Inductive Option
  (multiplier : nat)
  (underlying : Symbol)
  (kind : Kind)
  (expiration : Date)
  (strike : Amount) :=
  Contract.

(* The greeks change with respect to the price of the underlying. *)
Record Greeks := {
  delta    : R;
  gamma    : R;
  theta    : R;
  vega     : R;
  rho      : R;
  price    : Amount;            (* current price of the underlying *)
}.

(** jww (2020-04-10): Define function to compare theoretical model price. *)

Inductive Position (side : Side) (quantity : nat) (ty : Type) (cost : Amount) :=
  Pos.

(** Strategies

    Variables:
      p = premium
      s = strike
      e = expiration
      q = quantity

    Long call
    Short put
    Naked call
    Naked put
    Covered call                S - C
    Covered call collar         S - C(p) + P(p') where p < p'
    Cash-secured put            $ - P
    Married or collared put     S + P
    Poor man's covered call     C(e,s) - C(e',s') where e > e' && p < p'
    Poor man's covered put      P(e,s) - P(e',s') where e > e' && p > p'
    Verticals:
      Bull credit spread        P(s) - P(s') where p < p'
      Bull debit spread         C(s) - C(s') where p < p'
      Bear credit spread        C(s) - C(s') where p > p'
      Bear debit spread         P(s) - P(s') where p > p'
    Call calendar spread        C(e) - C(e') where e > e'
    Put calendar spread         P(e) - P(e') where e > e'
    Diagonal spread
    Box spread
    Call ratio spread
    Call ratio backspread
    Call ladder spread
    Straddle
    Strangle
    Butterfly
    Condor
    Iron Butterfly
    Iron Condor
 *)

Inductive Strategy :=
  | Hold : forall n mult sym cost,
      Position Long (n * mult) (Equity sym) cost -> Strategy
  | LongCall : forall n mult sym premium date strike,
      Position Long n (Option mult sym Call date strike) premium -> Strategy
  | LongPut : forall n mult sym premium date strike,
      Position Long n (Option mult sym Put date strike) premium -> Strategy
  | NakedCall : forall n mult sym premium date strike,
      Position Short n (Option mult sym Call date strike) premium -> Strategy
  | NakedPut : forall n mult sym premium date strike,
      Position Short n (Option mult sym Put date strike) premium -> Strategy
  | CoveredCall : forall n mult sym cost premium date strike,
      Position Long (n * mult) (Equity sym) cost ->
      Position Short n (Option mult sym Call date strike) premium -> Strategy
  | CashSecuredPut : forall n mult sym (capital : Amount) premium date strike,
      capital >= strike * INR n * INR mult ->
      Position Short n (Option mult sym Put date strike) premium -> Strategy.

Record RiskAnalysis := {
  max_profit : option Amount; (* None means infinity *)
  max_loss   : option Amount;
  breakeven  : Amount;        (* breakeven price for the underlying *)
}.

Definition AnalyzeRisk (strat : Strategy) : RiskAnalysis :=
  match strat with
  | Hold n mult sym cost equity =>
    {| max_profit := None
     ; max_loss   := Some (cost * INR n * INR mult)
     ; breakeven  := cost
     |}

  | LongCall n mult sym premium date strike opt =>
    {| max_profit := None
     ; max_loss   := Some premium
     ; breakeven  := strike + premium
     |}

  | LongPut n mult sym premium date strike opt =>
    {| max_profit := None
     ; max_loss   := Some premium
     ; breakeven  := strike - premium
     |}

  | NakedCall n mult sym premium date strike opt =>
    {| max_profit := Some premium
     ; max_loss   := None
     ; breakeven  := strike + premium
     |}

  | NakedPut n mult sym premium date strike opt =>
    {| max_profit := Some premium
     ; max_loss   := Some (strike * INR n * INR mult)
     ; breakeven  := strike - premium
     |}

  | CoveredCall n mult sym cost premium date strike equity opt =>
    {| (* Maximum profit is realized if assigned. *)
       max_profit := Some ((strike - cost) * INR n * INR mult + premium)
       (* Maximum loss is realized if the stock drops to zero. *)
     ; max_loss   := Some (cost * INR n * INR mult - premium)
     ; breakeven  := strike - premium
     |}

  | CashSecuredPut n mult sym capital premium date strike equity opt =>
    {| (* Maximum profit is realized if the option expires worthless. *)
       max_profit := Some premium
       (* Maximum loss is realized if the stock drops to zero. *)
     ; max_loss   := Some (strike * INR n * INR mult - premium)
     ; breakeven  := strike - premium
     |}
  end.

Inductive Effect := Profit | Loss.

Record Settlement := {
  effect : Effect;
  amount : Amount;
}.

Axiom Rdec_lt : R -> R -> bool.

(** Exercise, or close, a position immediately, with reference to the current
    price of the underlying, and the current value remaining on the contracts
    involed. *)
Definition Exercise (strat : Strategy) (price value : Amount) : Settlement :=
  match strat with
  | Hold n mult sym cost equity =>
    {| effect := if Rdec_lt cost price then Profit else Loss
     ; amount := Rabs (price - cost) * INR n * INR mult
     |}

  | LongCall n mult sym premium date strike opt =>
    {| effect := if Rdec_lt 0 value then Profit else Loss
     ; amount := value
     |}

  | LongPut n mult sym premium date strike opt =>
    {| effect := Profit
     ; amount := value
     |}

  | NakedCall n mult sym premium date strike opt =>
    {| effect := Profit
     ; amount := value
     |}

  | NakedPut n mult sym premium date strike opt =>
    {| effect := Profit
     ; amount := value
     |}

  | CoveredCall n mult sym cost premium date strike equity opt =>
    {| effect := Profit
     ; amount := value
     |}

  | CashSecuredPut n mult sym capital premium date strike equity opt =>
    {| effect := Profit
     ; amount := value
     |}
  end.

(** Equivalent Positions

     S =  C - P
     C =  S + P
     P =  C - S
    -S =  P - C
    -C = -S - P
    -P =  S - C
 *)

(** jww (2020-04-10): Define the concept of risk/reward equivalence. *)

Definition RiskEquivalent (x y : Strategy) : Prop := True.
