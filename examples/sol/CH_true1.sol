pragma solidity ^0.4.18;

contract Router
{
    address public Owner = msg.sender;
    address public DataBase;
    uint256 public Limit;
    
    
    function Set(address dataBase, uint256 limit)
    {
        // No authorization here.
        Limit = limit;
        DataBase = dataBase;
    }
    
    function()payable{}
    
    function transfer(address adr)
    payable
    {
        if(msg.value>Limit)
        {        
            require(DataBase.delegatecall(bytes4(sha3("AddToDB(address)")),msg.sender));
        }
    }
    
}
