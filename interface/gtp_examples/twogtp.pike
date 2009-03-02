#! /usr/bin/env pike

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 and      *
 * 2009 by the Free Software Foundation.                             *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define DUMP_GTP_PIPES		0


int
maximal_fixed_handicap(int board_size)
{
  if (board_size < 7)
    return 0;
  if (board_size % 2 == 1 && board_size >= 9)
    return 9;
  return 4;
}


float
result_to_float(string result)
{
  if (result[..1] == "W+")
    return (float) result[2..];
  if (result[..1] == "B+")
    return - (float) result[2..];
  return (float) result;
}


string
arrange_values_nicely(array(string) values, string delimiter)
{
  string result = "";
  for (int k = 0; k < sizeof(values); k++) {
    if (k > 0 && k % 12 == 0)
      result += "\n";
    result += delimiter + values[k];
  }

  return result + "\n";
}


string
list_sgf_positions(array(array(string)) positions, array(string) sgf_properties)
{
  string result = "";
  for (int k = 0; k < sizeof(positions); k++) {
    if (sizeof(positions[k]))
      result += sgf_properties[k] + arrange_values_nicely(positions[k], "");
  }

  return result;
}


string
nice_time(float _time)
{
  int time = (int) (_time * 10);
#ifdef __AUTO_BIGNUM__
  return sprintf("%d:%02d.%d", time / 600, (time % 600) / 10, time % 10);
#else
  return sprintf("%d:%02d", time / 600, (time % 600) / 10);
#endif
}


int
is_numeric(string str)
{
  str = String.trim_all_whites(str);
  if (str[0] == '+')
    str = str[1..];

  return ((string) ((int) str)) == str;
}


class GtpServer {
  int server_is_up;
  int board_size;
  private Stdio.File file_out;
  private Stdio.FILE file_in;
  int protocol_version;
  string color;
  string capitalized_color;
  string command_line;
  string full_engine_name;
  string random_seed;
  private float main_time;
  private float byo_yomi_time;
  private int byo_yomi_stones;
  private float time_left;
  private int stones_left;
  float total_used_time;
  private array(string) statistics;
  private array(string) reset;
  private array totals;


  void
  create(string _command_line, array(string) _statistics, array(string) _reset,
	 string _color,
	 float _main_time, float _byo_yomi_time, int _byo_yomi_stones)
  {
    file_out = Stdio.File();
    file_in = Stdio.FILE();
    set_color(_color);
    command_line = _command_line;

    array error = catch {
      Process.create_process(command_line / " ",
			     ([ "stdin" : file_out->pipe(),
				"stdout" : file_in->pipe() ]));
    };

    if (error) {
      werror(error[0]);
      werror("Command line was `%s'.\n", command_line);
      destruct(this_object());
    }
    else {
      array error = catch {
	array answer = send_command("protocol_version");
	protocol_version = answer[0] ? 1 : (int) answer[1];
	full_engine_name = get_full_engine_name();
	server_is_up = 1;
      };
      if (error) {
	werror("Engine `%s' crashed at startup.\nPerhaps command line is wrong.\n",
	       command_line);
	destruct(this_object());
      }
      else {
	main_time = _main_time;
	byo_yomi_time = _byo_yomi_time;
	byo_yomi_stones = _byo_yomi_stones;
	total_used_time = 0.0;

	statistics = _statistics;
	reset = _reset;
	totals = ({ 0 }) * sizeof(statistics);
      }
    }
  }


  void
  set_color(string _color)
  {
    color = _color;
    capitalized_color = String.capitalize(color);
  }


  void
  restart_if_crashed()
  {
    if (!server_is_up) {
      werror("Restarting engine `%s' playing %s.\n", command_line, color);
      Process.create_process(command_line / " ",
			     ([ "stdin" : file_out->pipe(),
				"stdout" : file_in->pipe() ]));
      server_is_up = 1;
    }
  }


  array
  send_command(string command)
  {
#if DUMP_GTP_PIPES
    werror("[%s%s] %s\n",
	   full_engine_name ? full_engine_name + ", " : "", color, command);
#endif

    command = String.trim_all_whites(command);
    sscanf(command, "%[0-9]", string id);
    if (command[0] == '#' || command == id)
      return ({ 0, "" });

    file_out->write("%s\n", command);
    string response = file_in->gets();
    if (!response) {
      server_is_up = 0;
      error("Engine `%s' playing %s crashed!", command_line, color);
    }

#if DUMP_GTP_PIPES
    werror("%s\n", response);
#endif

    array result;
    int id_length = strlen(id);
    if (response && response[..id_length] == "=" + id)
      result = ({ 0, response[id_length + 1 ..] });
    else if (response && response[..id_length] == "?" + id)
      result = ({ 1, response[id_length + 1 ..] });
    else
      result = ({ -1, response });

    result[1] = String.trim_all_whites(result[1]);
    while (1) {
      response = file_in->gets();

#if DUMP_GTP_PIPES
      werror("%s\n", response);
#endif

      if (response == "") {
	if (result[0] < 0) {
	  werror("Warning, unrecognized response to command `%s':\n", command);
	  werror("%s\n", result[1]);
	}
	return result;
      }
      result[1] += "\n" + response;
    }
  }


  int
  is_known_command(string command)
  {
    array answer = send_command("known_command " + command);
    return !answer[0] && answer[1] == "true";
  }


  string
  get_full_engine_name()
  {
    return send_command("name")[1] + " " + send_command("version")[1];
  }


  void
  set_board_size(int _board_size)
  {
    board_size = _board_size;
    send_command("boardsize " + board_size);
    if (protocol_version >= 2)
      send_command("clear_board");
    random_seed = get_random_seed();
  }


  array(string)
  fixed_handicap(int handicap)
  {
    array answer = send_command("fixed_handicap " + handicap);
    return answer[0] ? ({}) : answer[1] / " ";
  }


  array(string)
  place_free_handicap(int handicap)
  {
    array answer = send_command("place_free_handicap " + handicap);
    return answer[0] ? ({}) : answer[1] / " ";
  }


  void
  set_free_handicap(array(string) stones)
  {
    send_command("set_free_handicap " + (stones * " "));
  }


  array(string)
  set_handicap(int handicap, string handicap_mode,
	       GtpServer opponent, GtpServer arbiter)
  {
    if (handicap == 0)
      return ({});

    if (handicap_mode == "free") {
      array(string) stones = place_free_handicap(handicap);
      opponent->set_free_handicap(stones);
      if (arbiter)
	arbiter->set_free_handicap(stones);

      return stones;
    }
    else {
      opponent->fixed_handicap(handicap);
      if (arbiter)
	arbiter->fixed_handicap(handicap);

      return fixed_handicap(handicap);
    }
  }


  float
  get_komi()
  {
    return (float) (send_command("get_komi")[1]);
  }


  void
  set_komi(float komi)
  {
    send_command("komi " + komi);
  }


  array
  load_sgf(string sgf_file_name, int|string load_up_to)
  {
    array answer = send_command(sprintf("loadsgf %s %s", sgf_file_name,
					(string) load_up_to));
    board_size = (int) send_command("query_boardsize")[1];
    return answer;
  }


  void
  reset_engine()
  {
    foreach (reset, string reset_command)
      send_command(reset_command);

    initialize_time_control();
  }


  void
  initialize_time_control()
  {
    if (main_time < 0.0) {
      time_left = 0.0;
      return;
    }

    if (main_time > 0.0) {
      time_left = main_time;
      stones_left = 0;
    }
    else {
      time_left = byo_yomi_time;
      stones_left = byo_yomi_stones;
    }

    send_command(sprintf("time_settings %d %d %d", (int) main_time,
			 (int) byo_yomi_time, byo_yomi_stones));
  }


  string
  generate_move(int use_time_control)
  {
    string result;
    int time_notch = 0;

    if (use_time_control) {
      if (main_time >= 0.0) {
	send_command(sprintf("time_left %s %d %d", color,
			     (int) time_left, stones_left));
      }

#ifdef __AUTO_BIGNUM__
      time_notch = gethrtime();
#else
      time_notch = time();
#endif
    }

    if (protocol_version >= 2)
      result = send_command("genmove " + color)[1];
    else
      result = send_command("genmove_" + color)[1];

    if (use_time_control) {
#ifdef __AUTO_BIGNUM__
      time_left -= (gethrtime() - time_notch) / 1.0e6;
#else
      time_left -= time() - time_notch;
#endif
      if (main_time >= 0.0) {
	if (time_left < 0.0) {
	  if (stones_left > 0)
	    return "time";
	  else {
	    total_used_time += main_time;
	    time_left += byo_yomi_time;
	    stones_left = byo_yomi_stones;
	    if (time_left < 0.0)
	      return "time";
	  }
	}

	if (stones_left > 0 && --stones_left == 0) {
	  total_used_time += byo_yomi_time - time_left;
	  time_left = byo_yomi_time;
	  stones_left = byo_yomi_stones;
	}
      }
    }

    return result;
  }


  string
  get_time_left()
  {
    if (main_time < 0.0)
      return "";

    if (time_left < 0)
      return "lost on time";

    if (stones_left <= 0) {
      return sprintf("main time: %s / %s",
		     nice_time(time_left), nice_time(main_time));
    }

    return sprintf("byo-yomi time: %s / %s, stones: %d / %d",
		   nice_time(time_left), nice_time(byo_yomi_time),
		   stones_left, byo_yomi_stones);
  }


  void
  finalize_time_control()
  {
    if (main_time < 0.0)
      total_used_time += -time_left;
    else if (stones_left > 0)
      total_used_time += byo_yomi_time - time_left;
    else
      total_used_time += main_time - time_left;
  }


  void
  play(string color, string move)
  {
    if (protocol_version >= 2)
      send_command(sprintf("play %s %s", color, move));
    else
      send_command(sprintf("%s %s", color, move));
  }


  string
  get_random_seed()
  {
    array answer = send_command("get_random_seed");
    return answer[0] ? "unknown" : answer[1];
  }


  void
  set_random_seed(int|string seed)
  {
    random_seed = (string) seed;
    send_command("set_random_seed " + random_seed);
  }


  array(string)
  list_stones(string color)
  {
    return send_command("list_stones " + color)[1] / " ";
  }


  array(array(string))
  get_position_as_sgf()
  {
    if (!is_known_command("list_stones"))
      return ({});
    return ({ map(list_stones("white"), move_to_sgf_notation),
	      map(list_stones("black"), move_to_sgf_notation) });
  }


  array(string)
  final_status_list(string status)
  {
    array result = send_command("final_status_list " + status); 
    return result[0] ? ({}) : ((result[1] / "\n") * " ") / " " - ({""});
  }


  array(array(string))
  get_territory_as_sgf()
  {
    if (!is_known_command("final_status_list"))
      return ({});

    array(array(string)) result
      = ({ map(final_status_list("white_territory"), move_to_sgf_notation),
	   map(final_status_list("black_territory"), move_to_sgf_notation) });
    if (result[0] == ({}) && result[1] == ({}))
      return ({});

    if (is_known_command("color")) {
      array(string) dead_stones = final_status_list("dead");
      foreach (dead_stones, string stone) {
	switch (send_command("color " + stone)[1]) {
	case "black": result[0] += ({ move_to_sgf_notation(stone) }); break;
	case "white": result[1] += ({ move_to_sgf_notation(stone) }); break;
	}
      }
    }

    return result;
  }


  string
  show_board()
  {
    array answer = send_command("showboard");
    if (answer[0])
      return "\n";
    if (answer[1] != "" && answer[1][0] == '\n')
      return answer[1][1..] + "\n";
    return answer[1] + "\n";
  }


  void
  print_statistics(int dont_print_numerics)
  {
    for (int k = 0; k < sizeof(statistics); k++) {
      array command_result = send_command(statistics[k]);
      if (!command_result[0]) {
	if (is_numeric(command_result[1])) {
	  if (totals[k] != "")
	    totals[k] += (int) command_result[1];

	  if (dont_print_numerics)
	    continue;
	}
	else
	  totals[k] = "";

	write("%s (%s) statistic `%s': %s\n", full_engine_name, color,
	      statistics[k], command_result[1]);
      }
      else {
	werror("Couldn't acquire statistic `%s': engine failed with message \"%s\"\n",
	       statistics[k], command_result[1]);
      }
    }
  }


  void
  print_statistic_totals()
  {
    int first_total = 1;
    for (int k = 0; k < sizeof(statistics); k++) {
      if (totals[k] != "") {
	if (first_total) {
	  write("\n%s (%s) statistics totals:\n", full_engine_name, color);
	  first_total = 0;
	}

	write("`%s' total: %d\n", statistics[k], totals[k]);
      }
    }
  }


  string
  final_score()
  {
    array answer = send_command("final_score");
    return answer[0] ? "?" : answer[1];
  }


  string
  cpu_time()
  {
    if (is_known_command("cputime"))
      return send_command("cputime")[1];
    return "";
  }


  void
  quit()
  {
    send_command("quit");
  }


  string
  move_to_sgf_notation(string coordinates)
  {
    coordinates = lower_case(coordinates);
    if (coordinates == "pass")
      return "[]";

    int y = board_size - ((int) coordinates[1..]);
    int x = coordinates[0] - 'a';
    if (x > 'i' - 'a')
      x--;

    return sprintf("[%c%c]", 'a' + x, 'a' + y);
  }
};


class GtpGame {
  private GtpServer white;
  private GtpServer black;
  private GtpServer arbiter;
  private int verbose;
  private int black_to_play;
  private string sgf_header;
  private array(array(string)) territory;
  private int totals_only;


  void
  create(string command_line_white, string command_line_black,
	 string command_line_arbiter,
	 array(string) statistics_white, array(string) statistics_black,
	 array(string) reset_white, array(string) reset_black,
	 int _totals_only, int _verbose,
	 float main_time, float byo_yomi_time, int byo_yomi_stones)
  {
    verbose = _verbose;
    totals_only = _totals_only;
    white = GtpServer(command_line_white, statistics_white, reset_white,
		      "white", main_time, byo_yomi_time, byo_yomi_stones);
    if (white) {
      black = GtpServer(command_line_black, statistics_black, reset_black,
			"black", main_time, byo_yomi_time, byo_yomi_stones);

      if (black && command_line_arbiter != "") {
	arbiter = GtpServer(command_line_arbiter, ({}), ({}), "arbiter",
			    -1.0, 0.0, 0);
      }
      else
	arbiter = UNDEFINED;
    }

    if (!white || !black || (command_line_arbiter != "" && !arbiter))
      destruct(this_object());
  }


  void
  swap_engines()
  {
    GtpServer temp = white;
    white = black;
    black = temp;
    white->set_color("white");
    black->set_color("black");
  }


  void
  start_new_game(int board_size, int handicap, string handicap_mode,
		 float komi)
  {
    white->set_board_size(board_size);
    black->set_board_size(board_size);
    if (arbiter)
      arbiter->set_board_size(board_size);

    array(string) stones = black->set_handicap(handicap, handicap_mode,
					       white, arbiter);
    stones = map(stones, black->move_to_sgf_notation);

    white->set_komi(komi);
    black->set_komi(komi);
    if (arbiter)
      arbiter->set_komi(komi);

    black_to_play = (handicap == 0);
    sgf_header = sprintf("(;\nGM[1]FF[4]\nSZ[%d]HA[%d]KM[%.1f]\n",
			 board_size, handicap, komi);
    if (handicap)
      sgf_header += "AB" + arrange_values_nicely(stones, "");
  }


  array(string)
  play(string|int sgf_file_name)
  {
    array(string) sgf_moves = ({});
    array(array(string)) move_history = ({});
    string special_win = "";
    GtpServer player;
    GtpServer opponent;

    if (verbose)
      werror("\nBeginning a new game.\n");

    white->reset_engine();
    black->reset_engine();
    territory = UNDEFINED;

    array error = catch {
      int passes = 0;
      while (1) {
	player = black_to_play ? black : white;
	opponent = black_to_play ? white : black;

	string move = player->generate_move(1);
	string move_lower_case = lower_case(move);

	if (move_lower_case == "resign") {
	  if (verbose)
	    werror(player->capitalized_color + " resigns!\n");

	  special_win = sprintf("%c+Resign", opponent->capitalized_color[0]);
	  break;
	}

	if (move_lower_case == "time") {
	  if (verbose)
	    werror(player->capitalized_color + " loses on time!\n");

	  special_win = sprintf("%c+Time", opponent->capitalized_color[0]);
	  break;
	}

	opponent->play(player->color, move);
	sgf_moves += ({ sprintf("%c%s", player->capitalized_color[0],
				player->move_to_sgf_notation(move)) });
	if (arbiter)
	  move_history += ({ ({ player->color, move }) });

	if (move_lower_case == "pass") {
	  if (verbose)
	    werror("play " + player->capitalized_color + " pass\n");

	  if (++passes == 2)
	    break;
	}
	else {
	  if (verbose) {
	    string time_left = " (" + player->get_time_left() + ")";
	    if (time_left == " ()")
	      time_left = "";

	    werror("play %s %s%s\n", player->capitalized_color,
		   move, time_left);
	  }

	  passes = 0;
	}

	if (verbose >= 2) {
	  string board = white->show_board();
	  if (board == "")
	    board = black->show_board();
	  werror("%s\n", board);
	}
	black_to_play = !black_to_play;

	if (sgf_file_name)
	  write_sgf_file(sgf_file_name, sgf_moves, ({ "Void" }));
      }
    };

    white->finalize_time_control();
    black->finalize_time_control();

    array(string) result;
    if (error) {
      result = ({ "Void", error[0] });
      if (sgf_file_name)
	werror("The game will be saved in file `%s'.\n", sgf_file_name);

      white->restart_if_crashed();
      black->restart_if_crashed();
      if (arbiter)
	arbiter->restart_if_crashed();
    }
    else {
      if (special_win == "") {
	result = ({ white->final_score(), black->final_score() });

	if (result[1] == "?" || result[0] == result[1])
	  result = ({ result[0] });
	else if (result[0] == "?")
	  result = ({ result[1] });

	territory = player->get_territory_as_sgf();
	if (territory == ({}))
	  territory = opponent->get_territory_as_sgf();

	if (arbiter && (sizeof(result) == 2 || result[0] == "?")) {
	  foreach (move_history, array(string) move)
	    arbiter->play(move[0], move[1]);
	  result = ({ arbiter->final_score() });

	  if (territory == ({}))
	    territory = arbiter->get_territory_as_sgf();
	}
      }
      else
	result = ({ special_win });
    }

    if (sgf_file_name)
      write_sgf_file(sgf_file_name, sgf_moves, result);
    return result;
  }


  int
  init_endgame_contest(string endgame_file_name, int endgame_moves)
  {
    array(string) sgf_nodes;
    array error = catch {
      sgf_nodes = Stdio.read_file(endgame_file_name) / ";";
    };
    if (error) {
      werror(error[0]);
      return 0;
    }

    Regexp move = Regexp("\\<([BW]\\[[a-z][a-z]\\])");
    array(string) moves = ({});
    foreach (sgf_nodes, string sgf_node) {
      array move_groups = move->split(sgf_node);
      if (move_groups)
	moves += ({ move_groups[0] });
    }

    int load_up_to = max(sizeof(moves) - endgame_moves + 1, 1);
    array white_answer = white->load_sgf(endgame_file_name, load_up_to);
    array black_answer = black->load_sgf(endgame_file_name, load_up_to);
    array arbiter_answer = (arbiter
			    ? arbiter->load_sgf(endgame_file_name, load_up_to)
			    : ({0}));

    if (white_answer[0] || black_answer[0] || arbiter_answer[0]) {
      werror("File `%s' might be corrupt. Engines refuse to load it.\n",
	     endgame_file_name);
      return 0;
    }

    sgf_header = sprintf("(;\nGM[1]FF[4]\nSZ[%d]KM[%.1f]\n"
			 "C[Original game: `%s' loaded up to move %d]\n",
			 white->board_size, white->get_komi(),
			 endgame_file_name, load_up_to);
    array(array(string)) stones = white->get_position_as_sgf();
    if (!stones)
      stones = black->get_position_as_sgf();
    if (!stones && arbiter)
      stones = arbiter->get_position_as_sgf();
    if (!stones) {
      stones = ({ ({}), ({}) });
      moves = moves[..load_up_to - 2];
      foreach (moves, string move)
	stones[move[0] == 'B'] += ({ move[1..] });
    }

    sgf_header += list_sgf_positions(stones, ({ "AW", "AB" }));

    white->set_random_seed(0);
    black->set_random_seed(0);

    black_to_play = (black_answer[1] == "black");
    return load_up_to;
  }


  void
  reinit_endgame_contest(string endgame_file_name, int load_up_to)
  {
    swap_engines();
    white->load_sgf(endgame_file_name, load_up_to);
    array black_answer = black->load_sgf(endgame_file_name, load_up_to);

    if (arbiter)
      arbiter->load_sgf(endgame_file_name, load_up_to);

    white->set_random_seed(0);
    black->set_random_seed(0);
    black_to_play = (black_answer[1] == "black");
  }


  void
  write_sgf_file(string sgf_file_name, array(string) sgf_moves,
		 array(string) result)
  {
    string sgf_data = sprintf("PW[%s (random seed %s)]\nPB[%s (random seed %s)]\n"
			      "RU[Japanese]\nRE[%s]\n",
			      white->full_engine_name, white->random_seed,
			      black->full_engine_name, black->random_seed,
			      result[0]);
    if (sizeof(result) > 1) {
      sgf_data += sprintf("GC[%s]\n", result[0] == "Void" ? result[1] :
			  sprintf("Engines disagreed on results:\n"
				  "White claimed %s\nBlack claimed %s\n",
				  result[0], result[1]));
    }

    sgf_data += arrange_values_nicely(sgf_moves, ";");
    if (sizeof(result) == 1) {
      if (territory)
	sgf_data += ";" + list_sgf_positions(territory, ({ "TW", "TB" }));
    }
    else if (result[0] == "Void")
      sgf_data += ";C[" + result[1] + "]\n";
    sgf_data += ")\n";

    array error = catch {
      Stdio.write_file(sgf_file_name, sgf_header + sgf_data);
    };

    if (error)
      werror(error[0]);
  }


  void
  print_time_report()
  {
    string cpu_time_white = white->cpu_time();
    string cpu_time_black = black->cpu_time();
    if (cpu_time_white != "")
      write("White: %ss CPU time.\n", cpu_time_white);
    if (cpu_time_black != "")
      write("Black: %ss CPU time.\n", cpu_time_black);

    write("\nTime control report (wall move generation time):\n");
    write("  White: %s\n", nice_time(white->total_used_time));
    write("  Black: %s\n", nice_time(black->total_used_time));
  }


  void
  print_statistics()
  {
    white->print_statistics(totals_only);
    black->print_statistics(totals_only);
  }


  void
  print_statistic_totals()
  {
    white->print_statistic_totals();
    black->print_statistic_totals();
  }


  void
  finalize()
  {
    white->quit();
    black->quit();
  }
}


void
run_twogtp_match(GtpGame game, int num_games, int board_size, int handicap,
		 string handicap_mode, int adjust_handicap, float komi,
		 int verbose, string|int sgf_base, int skip_games)
{
  int white_wins = 0;
  int black_wins = 0;
  int jigos = 0;
  int result_unknown = 0;
  int disagreed_games = 0;
  int last_streak = 0;
  int last_to_win = '0';
  array(array(string)) results = ({});

  for (int k = skip_games; k < skip_games + num_games; k++) {
    game->start_new_game(board_size, handicap, handicap_mode, komi);
    array(string) result
      = game->play(sgf_base ? sprintf("%s%03d.sgf", sgf_base, k + 1) : 0);

    write("Game %d: %s\n", k + 1, result * "  ");
    game->print_statistics();
    results += ({result});

    if (sizeof(result) == 1 || (result[0][0] == result[1][0])) {
      switch (result[0][0]) {
      case 'W':
      case 'B':
	if (result[0][0] == 'W')
	  white_wins++;
	else
	  black_wins++;

	if (result[0][0] == last_to_win)
	  last_streak++;
	else {
	  last_to_win = result[0][0];
	  last_streak = 1;
	}
	break;

      case '0': jigos++; break;

      default:
	result_unknown++;
	last_to_win = '0';
      }
    }
    else {
      result_unknown++;
      last_to_win = '0';
    }

    if (adjust_handicap && last_streak == adjust_handicap) {
      if (result[0][0] == 'W') {
	if (handicap_mode == "free"
	    || handicap < maximal_fixed_handicap(board_size)) {
	  if (++handicap == 1)
	    handicap = 2;
	  write("White wins too often. Increasing handicap to %d.\n", handicap);
	}
      }
      else {
	if (handicap == 0) {
	  handicap = 2;
	  if (handicap_mode == "fixed")
	    handicap = min(maximal_fixed_handicap(board_size), 2);
	  game->swap_engines();
	  write("Black looks stronger than white. Swapping colors and setting handicap to %d.\n",
		handicap);
	}
	else {
	  if (--handicap == 1)
	    handicap = 0;
	  write("Black wins too often. Decreasing handicap to %d.\n",
		handicap);
	}
      }

      last_streak = 0;
    }

    if (sizeof(result) == 2 && result[0] != "Void")
      disagreed_games++;
  }

  if (verbose) {
    write("\n");
    for (int k = 0; k < num_games; k++)
      write("Game %d: %s\n", skip_games + k + 1, results[k] * "  ");
  }

  write("\nTotal %d game(s).\nWhite won %d. Black won %d.",
	num_games, white_wins, black_wins);
  if (jigos)
    write(" %d jigos.", jigos);
  if (result_unknown)
    write(" Results of %d game(s) are unknown.", result_unknown);
  write("\n");
  if (disagreed_games)
    write("Engines disagreed on results of %d game(s).\n", disagreed_games);

  game->print_time_report();
  game->print_statistic_totals();
  game->finalize();
}


void
endgame_contest(GtpGame game, int endgame_moves, array(string) endgame_files,
		int verbose, string|int sgf_base, int skip_games)
{
  array(string) differences = ({});
  for (int k = skip_games; k < sizeof(endgame_files); k++) {
    int load_up_to = game->init_endgame_contest(endgame_files[k], endgame_moves);
    if (load_up_to) {
      if (verbose)
	werror("Replaying game `%s'.\n", endgame_files[k]);

      array(string) result1
	= game->play(sgf_base ? sprintf("%s%03d_1.sgf", sgf_base, k + 1) : 0);
      game->print_statistics();
      game->reinit_endgame_contest(endgame_files[k], load_up_to);

      array(string) result2
	= game->play(sgf_base ? sprintf("%s%03d_2.sgf", sgf_base, k + 1) : 0);
      game->print_statistics();
      game->swap_engines();

      write("%s: ", endgame_files[k]);
      if (sizeof(result1) > 1 || sizeof(result2) > 1) {
	write("can't determine difference, engines disagreed on results.\n");
	write("\t%s\n", result1 * " ");
	write("\t%s\n", result2 * " ");
	differences += ({ "unknown" });
      }
      else {
	string difference = sprintf("%+.1f", (result_to_float(result1[0])
					      - result_to_float(result2[0])));
	if (difference == "+0.0") {
	  write("same result: %s\n", result1[0]);
	  differences += ({ "0" });
	}
	else {
	  write("%s %s; difference %s.\n", result1[0], result2[0], difference);
	  differences += ({ difference });
	}
      }
    }
  }

  int white_wins = 0;
  int black_wins = 0;
  write("\n");
  foreach (differences, string difference) {
    write(difference + "\n");
    if (difference[0] == '+')
      white_wins++;
    else if (difference[0] == '-')
      black_wins++;
  }

  write("\nTotal %d game(s) replayed. White won %d. Black won %d.\n",
	sizeof(differences), white_wins, black_wins);
  game->print_time_report();
  game->print_statistic_totals();
  game->finalize();
}


string help_message =
  "Usage: %s [OPTION]... [FILE]...\n\n"
  "Runs either a match or endgame contest between two GTP engines.\n"
  "`--white' and `--black' options are mandatory.\n\n"
  "Options:\n"
  "  -w, --white=COMMAND_LINE\n"
  "  -b, --black=COMMAND_LINE      command lines to run the two engines with.\n\n"
  "  -A, --arbiter=COMMAND_LINE    command line to run arbiter--program that will\n"
  "                                score disputed games--with.\n"
  "      --help                    display this help and exit.\n"
  "      --help-statistics         display help on statistics options and exit.\n"
  "  -v, --verbose=LEVEL           1 - print moves, 2 and higher - draw boards.\n"
  "      --no-sgf                  do not create SGF game recods.\n"
  "      --sgf-base=FILENAME       create SGF files with FILENAME as base (default\n"
  "                                is `twogtp' or `endgame' depending on mode).\n"
  "  -m, --match                   runs a match between the engines (the default).\n"
  "  -e, --endgame=MOVES           runs an endgame contest instead of a match.\n"
  "  -c, --continue                continue a match or endgame contest.\n\n"
  "Options valid only in match mode:\n"
  "  -g, --games=GAMES             number of games in the match (one by default).\n"
  "  -s, --board-size=SIZE         the board size for the match (default is 19).\n"
  "  -h, --handicap=STONES         fixed handicap for the black player.\n"
  "  -f, --free-handicap=STONES    free handicap for the black player.\n"
  "  -a, --adjust-handicap=LENGTH  use simple adjusting scheme: change handicap\n"
  "                                by 1 after LENGTH wins in a row.\n"
  "  -k, --komi=KOMI               the komi to use.\n\n"
  "Time control options:\n"
  "  -t, --main-time=TIME          main time for a game (default is forever with\n"
  "                                no byo-yomi or zero otherwise).\n"
  "  -B, --byo-yomi-time=TIME      byo-yomi time for a game (zero by default).\n"
  "                                TIMEs are in minutes and can be fractional.\n"
  "  -S, --byo-yomi-stones=STONES  stones to be played in a byo-yomi period\n"
  "                                (default is 25).\n\n"
  "Default is no handicap. Komi defaults to 5.5 with no handicap or 0.5 with\n"
  "nonzero handicap. Note that `--adjust-handicap' option not only can change\n"
  "handicap, but can also swap engines' colors if black appears stronger.\n\n"
  "FILEs are only used in endgame contest mode. They must be non-branched SGF\n"
  "game records. In endgame contest mode the FILEs are loaded into the engines\n"
  "excluding last non-pass MOVES specified with `--endgame' option. For each of\n"
  "the FILEs two games are played with alternating colors and the difference in\n"
  "results is determined.\n\n"
  "Option `--continue' allows to have a continuous set of game records for\n"
  "several script runs. It restarts a match or endgame contest skipping all\n"
  "games for which game records already exist. In case of an endgame contest\n"
  "it also skips appropriate number of FILEs.\n";

string help_statistics_message =
  "Engine statistics options:\n"
  "  --statistics=COMMANDS\n"
  "  --statistics-white=COMMANDS\n"
  "  --statistics-black=COMMANDS   COMMANDS is a semicolon separated list of GTP\n"
  "                                commands to be executed after each game; if\n"
  "                                engines' responses appear to be numeric, totals\n"
  "                                are printed after all games are played.\n"
  "  --reset=COMMANDS\n"
  "  --reset-white=COMMANDS\n"
  "  --reset-black=COMMANDS        semicolon separated list of GTP commands needed\n"
  "                                to reset statistics before each game.\n\n"
  "  --totals-only                 don't print numeric statistics after each game.\n"
  "Note that you can use `--statistics' and `--reset' options to acquire similar\n"
  "statistics from both engines (provided they both understand the commands). If\n"
  "you use color-specific options together with common options, both command lists\n"
  "are used as one would expect.\n";


int
main(int argc, array(string) argv)
{
  string hint = sprintf("Try `%s --help' for more information.\n",
			basename(argv[0]));

  if (Getopt.find_option(argv, UNDEFINED, "help")) {
    write(help_message, basename(argv[0]));
    return 0;
  }

  if (Getopt.find_option(argv, UNDEFINED, "help-statistics")) {
    write(help_statistics_message);
    return 0;
  }

  string white = Getopt.find_option(argv, "w", "white", UNDEFINED, "");
  if (white == "") {
    werror("White player is not specified.\n" + hint);
    return 1;
  }

  string black = Getopt.find_option(argv, "b", "black", UNDEFINED, "");
  if (black == "") {
    werror("Black player is not specified.\n" + hint);
    return 1;
  }

  string arbiter = Getopt.find_option(argv, "A", "arbiter", UNDEFINED, "");

  int verbose = (int) Getopt.find_option(argv, "v", "verbose",
					 UNDEFINED, "0");
  Getopt.find_option(argv, "m", "match");
  int endgame_moves = (int) Getopt.find_option(argv, "e", "endgame",
					       UNDEFINED, "0");
  int mode = (endgame_moves > 0);

  string|int sgf_base = 0;
  if (!Getopt.find_option(argv, UNDEFINED, "no-sgf")) {
    sgf_base = Getopt.find_option(argv, UNDEFINED, "sgf-base",
				  UNDEFINED, mode ? "endgame" : "twogtp");
  }
  else {
    if (Getopt.find_option(argv, UNDEFINED, "sgf-base"))
      werror("Warning: `--no-sgf' option specified, `--sgf-base' has no effect");
  }

  int skip_games = Getopt.find_option(argv, "c", "continue");
  if (skip_games) {
    for (skip_games = 0; ; skip_games++) {
      if (!Stdio.is_file(sprintf("%s%03d%s.sgf", sgf_base, skip_games + 1,
				 mode ? "_1" : "")))
	break;
    }
  }

  float main_time = (float) Getopt.find_option(argv, "t", "main-time",
					       UNDEFINED, "0") * 60.0;
  float byo_yomi_time = (float) Getopt.find_option(argv, "B", "byo-yomi-time",
						   UNDEFINED, "0") * 60.0;
  if (main_time == 0.0 && byo_yomi_time <= 0.0)
    main_time = -1.0;
  int byo_yomi_stones = (int) Getopt.find_option(argv, "S", "byo-yomi-stones",
						 UNDEFINED, "25");
  byo_yomi_stones = max(byo_yomi_stones, 1);

  string|int statistics_value
    = Getopt.find_option(argv, UNDEFINED, "statistics", UNDEFINED, "");
  string|int statistics_white_value
    = Getopt.find_option(argv, UNDEFINED, "statistics-white", UNDEFINED, "");
  string|int statistics_black_value
    = Getopt.find_option(argv, UNDEFINED, "statistics-black", UNDEFINED, "");

  array(string) statistics_white = ({});
  array(string) statistics_black = ({});

  if (statistics_value && statistics_value != "") {
    statistics_white = map(statistics_value / ";", String.trim_all_whites);
    statistics_black = map(statistics_value / ";", String.trim_all_whites);
  }

  if (statistics_white_value && statistics_white_value != "") {
    statistics_white |= map(statistics_white_value / ";",
			    String.trim_all_whites);
  }

  if (statistics_black_value && statistics_black_value != "") {
    statistics_black |= map(statistics_black_value / ";",
			    String.trim_all_whites);
  }

  string|int reset_value
    = Getopt.find_option(argv, UNDEFINED, "reset", UNDEFINED, "");
  string|int reset_white_value
    = Getopt.find_option(argv, UNDEFINED, "reset-white", UNDEFINED, "");
  string|int reset_black_value
    = Getopt.find_option(argv, UNDEFINED, "reset-black", UNDEFINED, "");

  array(string) reset_white = ({});
  array(string) reset_black = ({});

  if (reset_value && reset_value != "") {
    reset_white = map(reset_value / ";", String.trim_all_whites);
    reset_black = map(reset_value / ";", String.trim_all_whites);
  }

  if (reset_white_value && reset_white_value != "")
    reset_white |= map(reset_white_value / ";", String.trim_all_whites);

  if (reset_black_value && reset_black_value != "")
    reset_black |= map(reset_black_value / ";", String.trim_all_whites);

  int totals_only = (Getopt.find_option(argv, UNDEFINED, "totals-only") != 0);

  if (!mode) {
    string handicap_mode = "fixed";
    int handicap = 0;

    int games = (int) Getopt.find_option(argv, "g", "games", UNDEFINED, "1");
    games = max(games, 1);
    int board_size = (int) Getopt.find_option(argv, "s", "board-size",
					      UNDEFINED, "19");
    if (board_size < 1 || 25 < board_size) {
      werror("GTP only supports boards with size from 1 to 25.\n");
      return 1;
    }

    int fixed_handicap = (int) Getopt.find_option(argv, "h", "handicap",
						  UNDEFINED, "-1");

    int free_handicap = (int) Getopt.find_option(argv, "f", "free-handicap",
						 UNDEFINED, "-1");

    if (fixed_handicap >= 0 && free_handicap >= 0) {
      werror("Fixed and free handicaps are mutually exclusive.\n" + hint);
      return 1;
    }

    if (fixed_handicap >= 0) {
      int maximum = maximal_fixed_handicap(board_size);
      if (fixed_handicap > maximum) {
	write("Maximal allowed handicap for board size %d is %d.\n",
	      board_size, maximum);
	return 1;
      }

      handicap = fixed_handicap;
      handicap_mode = "fixed";
    }
    else if (free_handicap >= 0) {
      handicap = free_handicap;
      handicap_mode = "free";
    }

    if (handicap == 1) {
      werror("Warning: handicap 1 is not allowed, falling back on handicap 0.\n");
      handicap = 0;
    }

    int adjust_handicap = (int) Getopt.find_option(argv, "a", "adjust-handicap",
						   UNDEFINED, "0");
    adjust_handicap = max(adjust_handicap, 0);

    float komi = handicap ? 0.5 : 5.5;
    komi = (float) Getopt.find_option(argv, "k", "komi",
				      UNDEFINED, (string) komi);

    if (sizeof(Getopt.get_args(argv)) != 1) {
      werror("Unrecognized input in command line.\n" + hint);
      return 1;
    }


    GtpGame game = GtpGame(white, black, arbiter,
			   statistics_white, statistics_black,
			   reset_white, reset_black, totals_only,
			   verbose, main_time, byo_yomi_time, byo_yomi_stones);
    if (game) {
      run_twogtp_match(game, games, board_size, handicap, handicap_mode,
		       adjust_handicap, komi, verbose, sgf_base, skip_games);
    }
  }
  else {
    array(string) endgame_files = Getopt.get_args(argv)[1..];
    if (sizeof(endgame_files) == 0) {
      werror("No SGF files specified for endgame contest.\n" + hint);
      return 1;
    }

    GtpGame game = GtpGame(white, black, arbiter,
			   statistics_white, statistics_black,
			   reset_white, reset_black, totals_only,
			   verbose, main_time, byo_yomi_time, byo_yomi_stones);
    if (game) {
      endgame_contest(game, endgame_moves, endgame_files,
		      verbose, sgf_base, skip_games);
    }
  }
}
