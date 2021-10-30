pragma solidity ^0.4.16;

contract C {
  address owner = 0;
  uint private stateA = 0;
  uint private stateB = 0;
  uint CONST = 32;

  function C() public { // Constructor
    owner = msg.sender;
  }

  function f(uint x) {
    if (msg.sender == owner) { stateA = x; }
  }

  function g(uint y) {
    if (stateA % CONST == 1) {
      stateB = y - 10;
    }
  }

  function h() {
    if (stateB == 62) { bug(); }
  }

  function bug() private {
    assert(false);
  }
}
