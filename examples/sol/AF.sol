pragma solidity 0.4.26;

contract AssertionFailure {
    int256 private x;
    int256 private y;
    constructor() public {
        x = 0;
        y = 0;
    }
    function Bar() public view {
        if(x == 42) {
            // ASSERTION FAILURE
            assert(false);
        }
    }
    function SetY(int256 ny) public { y = ny; }
    function CopyY() public { x = y; }
}