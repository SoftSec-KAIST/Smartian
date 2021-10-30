pragma solidity 0.4.13;

contract MerdeToken {
    address public owner;

    function MerdeToken() {
        owner = msg.sender;
    }

    modifier onlyOwner {
        require(msg.sender == owner);
        _;
    }

    uint[] public bonusCodes;

    function pushBonusCode(uint code) onlyOwner {
        bonusCodes.push(code);
    }

    function popBonusCode() onlyOwner {
        require(bonusCodes.length >= 0);
        bonusCodes.length--; // No pop() method?
    }

    function modifyBonusCode(uint index, uint update) onlyOwner {
        require(index < bonusCodes.length);
        bonusCodes[index] = update;
    }
}