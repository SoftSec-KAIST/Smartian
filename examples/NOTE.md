# Alarms that should be found from each example.

- motiv: AF, IB\_all

- constructor: IB\_all

- AF: AF

- AW: AW

- BD\_true1: BD\_all

  Block number.

- BD\_true2: BD\_all

  Block timestamp.

- BD\_false: BD\_ilf, BD\_sfuzz, BD\_myth, BD\_mant

  BD\_ilf occurs because ILF raises alarms even if a state-affected JUMPI and
  ether-sending CALL are in different transactions.

  BD\_sfuzz occurs because it mistakes msg.value > 0 as ether transfer. This
  makes sFuzz to raise an alarm at the fallback function.

  BD\_myth and BD\_mant occur because these tools do not check whether there is
  a taint flow to CALL instructions.

- CH\_true1, CH\_true2: CH

  Dangerous delegatecall and arbitrary jump, respectively.

- CH\_false: None

  No alarm because we check for the deployer's transaction (contract deployer
  must be allowed to arbitrarily decide which external contract to execute with
  DELEGATECALL).

- IB: IB\_all

- ME\_true: ME\_all, EL

  EL occurs due to a mistake in the constructor name.

- MS\_true: MS

- MS\_false: None

  No alarm because we check for non-zero ether value.

- RE: RE\_all

- SC: SC

- TO: TO
