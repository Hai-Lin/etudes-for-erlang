
-module(bank).
-export([account/1]).

account(Balance) ->
  Char = io:get_line("D)eposit, W)ithdraw, B)alance, Q)uit: "),
  case hd(Char) of
    $D -> deposit_handler(Balance);
    $d -> deposit_handler(Balance);
    $W -> withdraw_handler(Balance);
    $w -> withdraw_handler(Balance);
    $B -> balance_handler(Balance);
    $b -> balance_handler(Balance);
    $Q ->
      io:format("Quitting...~n");
    $q ->
      io:format("Quitting...~n")
  end.

deposit_handler(Balance) ->
  Deposit = number:get_number("Amount to deposit: "),
  if Deposit < 0 ->
      error_logger:error_msg("Deposit may not be less than zero.~n"),
      account(Balance);
    true ->
      error_logger:info_msg("Successfully deposit ~p.~n",[Deposit]),
      account(Balance + Deposit)
  end.

withdraw_handler(Balance) ->
  Withdraw = number:get_number("Amount to withdraw: "),
  if Withdraw > Balance ->
      error_logger:error_msg("Overdraw ~p from balance ~p~n", [Withdraw, Balance]),
      account(Balance);
    Withdraw < 0 ->
      error_logger:error_msg("Withdraw may not be less than zero.~n"),
      account(Balance);
    true ->
      error_logger:info_msg("Successfully withdraw ~p.~n",[Withdraw]),
      account(Balance - Withdraw)
  end.

balance_handler(Balance) ->
  error_logger:info_msg("Balance inquiry ~p.~n",[Balance]),
  account(Balance).









