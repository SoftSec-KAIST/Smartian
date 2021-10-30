namespace Smartian

open Nethermind.Core
open Utils

type Hash = uint64

type Signedness = Signed | Unsigned

type Sign = Positive  | Negative | Zero

/// Direction that the cursor of a 'Seed' should move toward.
type Direction = Stay | Left | Right

/// The type of agent contract. Note that Smartian, Contractfuzzer, sFuzz have
/// slightly different agent contract code. We support only sFuzz for now.
type AgentType =
  | NoAgent
  | SmartianAgent of Address
  | SFuzzAgent of Address


type Sender =
  | TargetOwner
  | NormalUser1
  | NormalUser2
  | NormalUser3

module Sender =

  let pick () =
    pickFromList [TargetOwner; NormalUser1; NormalUser2; NormalUser3]