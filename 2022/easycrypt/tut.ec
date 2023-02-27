require import Distr AllCore FSet SmtMap DProd List.

(* A Stateful Random Generator *)

type seed.

op dseed: seed distr.
axiom dseed_ll: is_lossless dseed.

type state.

op dstate: state distr.
axiom dstate_uf: is_uniform dstate.
axiom dstate_fu: is_full dstate.

type output.

op dout: output distr.
axiom dout_uf: is_uniform dout.

op Fc : seed -> state -> state * output.

module SRG = {
  var s : seed
  var st: state

  proc init(): unit = {
    s <$ dseed;
    st <$ dstate;
  }

  proc next(): output = {
    var r;
    (st,r) <- Fc s st;
    return r;
  }
}.

(* A Pseudo-Random Generator *)

theory PRG.
  type output.
  op dout: output distr.

  module type PRG = {
    proc init(): unit
    proc next(): output
  }.

  module type PRGA = {
    proc next(): output
  }.

  module type Distinguisher(G: PRGA) = { proc distinguish(): bool }.

  module IND(G: PRG, D: Distinguisher) = {
    module D = D(G)

    proc main(): bool = {
      var b;

      G.init();
      b <@ D.distinguish();
      return b;
    }
  }.

  module PRGi: PRG, PRGA = {
    proc init(): unit = {}
    proc next(): output = { var r; r <$ PRG.dout; return r; }
  }.

end PRG.

(* PRF *)

theory PRF.
  type D.

  type R.

  type K.

  op dK: K distr.
  axiom dK_ll: is_lossless dK.

  op F: K -> D -> R.

  module PRFr = {
    var k: K
    proc init(): unit = { k <$ dK; }
    proc f(x : D): R = { return F k x; }
  }.

  op uR: R distr.
  axiom uR_uf: is_uniform uR.

  module PRFi = {
    var m : (D, R) fmap

    proc init(): unit = {
      m <- empty;
    }

    proc f (x : D) : R = {
      var r: R;
      if (! x \in m) {
        r <$ uR;
        m.[x] <- r;
      }
      return (oget m.[x]);
    }
  }.

  module type PRF = {
    proc init() : unit
    proc f(x : D) : R
  }.

  module type PRFA = {
    proc f(x : D) : R
  }.

  module type Distinguisher (F: PRFA) = {
    proc distinguish (): bool
  }.

  module IND(F: PRF, D: Distinguisher) = {
    module D = D(F)

    proc main(): bool = {
      var b;

      F.init();
      b <@ D.distinguish();
      return b;
    }
  }.

end PRF.

clone PRF as PRFa
with
  type D <- state,
  type R <- state * output,
  type K <- seed,
  op dK <- dseed,
  op F <- Fc,
  op uR <- dstate `*` dout (* product distribution *)
proof dK_ll by apply dseed_ll.

module IND_PRF = PRFa.IND.
module PRFc = PRFa.PRFr.
module PRFi = PRFa.PRFi.

clone PRG as PRGa
with
  type output <- output,
  op dout <- dout.

module IND_PRG = PRGa.IND.
module PRGi = PRGa.PRGi.

(* Proof sketch *)

module D_PRF(D: PRGa.Distinguisher, F: PRFa.PRFA) = {
  var log: state list

  module PRGp = {
    proc init(): unit = {
      SRG.st <$ dstate;
      log <- [];
    }

    proc next(): output = {
      var r;

      log <- SRG.st::log;
      (SRG.st, r) <@ F.f(SRG.st);
      return r;
    }
  }

  proc distinguish = IND_PRG(PRGp, D).main
}.
