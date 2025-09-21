open! Core
open! Physics_engine_lib
open! Async

let () =
  Run.run () |> don't_wait_for;
  Core.never_returns (Async.Scheduler.go ())
;;
