#!/usr/bin/env pike

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This distributed with GNU Go, a go program.			     * 
 *                                                                   *
 * Copyright 2003 and 2004                   			     *
 * by the Free Software Foundation.                                  *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 2             *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 59 Temple Place - Suite 330,           *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


class GtpResponse
{
    string status;
    string text;
    
    void create(string|void _status, string|void _text)
    {
	status = _status;
	text = _text;
    }

    int success()
    {
	return status == "=";
    }

    int failure()
    {
	return status == "?";
    }
}

class SimpleGtp
{
    static object engine;
    static function crash_callback = 0;
    function trace_callback = 0;
    
    Stdio.File engine_in;
    Stdio.FILE engine_out;
    Stdio.FILE engine_err;
    
    int id_number = 1;
    
    // Send a command to the go engine.
    GtpResponse send_command(string s)
    {
	s = id_number + " " + s + "\n";
	id_number++;
	engine_in->write(s);
	int first_line = 1;
	GtpResponse response = GtpResponse();
	while (1)
	{
	    string s = engine_out->gets();
	    if (!s)
	    {
		// FIXME: This is probably not adequate.
		if (crash_callback)
		    crash_callback();
		engine_in->close();
		engine_out->close();
		break;
	    }
	    
	    if (first_line)
	    {
		if (s == "")
		    continue;
		response->status = s[0..0];
		first_line = 0;
		sscanf(s[1..], "%*d %s", response->text);
	    }
	    else
	    {
		if (s == "")
		    break;
		response->text += "\n" + s;
	    }
	}
	return response;
    }

    // Tell the program to stop playing.
    void quit()
    {
	crash_callback = 0;
	send_command("quit");
    }
    
    static void program_trace_reader()
    {
	while (1)
	{
	    string s = engine_err->gets();
	    if (!s)
		break;
	    if (trace_callback)
		trace_callback(s);
	}
	
	engine_err->close();
    }
    
    void create(array(string) program_start_array,
		function|void crash_callback_)
    {
	crash_callback = crash_callback_;
	engine_in = Stdio.File();
	engine_out = Stdio.FILE();
	engine_err = Stdio.FILE();
	engine = Process.create_process(program_start_array,
					(["stdin":engine_in->pipe(),
					  "stdout":engine_out->pipe(),
					  "stderr":engine_err->pipe()]));
	thread_create(program_trace_reader);
    }
}

class Goban
{
    constant letters = "ABCDEFGHJKLMNOPQRSTUVWXYZ" / "";
    int boardsize;
    int gobansize;
    int spacing;
    int offset;
    Image.Fonts.Font font;
    Image.Fonts.Font small_font;
    array(string) white_stones;
    array(string) black_stones;

    class Markup(string vertex, string symbol, string color)
    {
	void draw(Image.Image image)
	{
	    vertex = upper_case(vertex);
	    if (vertex == "PASS")
		return;
	    [int x, int y] = vertex_to_pixel_coord(vertex);
	    image->setcolor(@Image.Color(color)->rgb());
	    if (sscanf(symbol, "text:%s", string text) == 1)
	    {
		Image.Image text_image = font->write(text);
		if (text_image->xsize() >= 0.9*spacing)
		    text_image = small_font->write(text);
		int width = text_image->xsize();
		int height = text_image->ysize();
		image->paste(text_image * 0 + ({220, 150, 50}), x - width / 2,
			     y - height / 2);
		image->paste_alpha_color(text_image, x - width / 2,
					 y - height / 2);
	    }
	    else
	    {
		switch (symbol)
		{
		case "circle":
		    image->circle(x, y, spacing / 3, spacing / 3);
		    break;
		case "square":
		    int delta = spacing / 4;
		    image->line(x - delta, y - delta, x - delta, y + delta);
		    image->line(x - delta, y + delta, x + delta, y + delta);
		    image->line(x + delta, y + delta, x + delta, y - delta);
		    image->line(x + delta, y - delta, x - delta, y - delta);
		    break;
		case "big_square":
		    delta = spacing / 2 - 1;
		    image->line(x - delta, y - delta, x - delta, y + delta);
		    image->line(x - delta, y + delta, x + delta, y + delta);
		    image->line(x + delta, y + delta, x + delta, y - delta);
		    image->line(x + delta, y - delta, x - delta, y - delta);
		    break;
		case "triangle":
		    delta = spacing / 2 - 1;
		    image->line(x - delta, y + delta, x + delta, y + delta);
		    image->line(x + delta, y + delta, x, y - delta);
		    image->line(x, y - delta, x - delta, y + delta);
		    break;
		case "dot":
		    draw_disc(image, x, y, spacing / 6);
		    break;
		case "small_dot":
		    draw_disc(image, x, y, spacing / 9);
		    break;
		case "big_dot":
		    draw_disc(image, x, y, spacing / 4);
		    break;
		case "stone":
		    draw_disc(image, x, y, spacing / 2, 0.5);
		    break;
		default:
		    werror("Unknown symbol: " + symbol + "\n");
		    break;
		}
	    }
	}
    }

    array(Markup) markups;
    
    static void create(int boardsize_, int gobansize_)
    {
	boardsize = boardsize_;
	gobansize = gobansize_;
	spacing = (int) (gobansize / (boardsize + 1.5));
	offset = (gobansize - spacing * (boardsize - 1)) / 2;
	font = Image.Fonts.Font(font_filename, spacing / 2);
	small_font = Image.Fonts.Font(font_filename, spacing / 3);
	white_stones = ({});
	black_stones = ({});

	markups = ({});
    }

    void add_stones(string color, string|array(string) stones)
    {
	if (color == "WHITE")
	    white_stones |= Array.arrayify(stones);
	else if (color == "BLACK")
	    black_stones |= Array.arrayify(stones);
    }

    int occupied(string vertex)
    {
	return (has_value(white_stones, vertex)
		|| has_value(black_stones, vertex));
    }
    
    void add_symbol(string vertex, string name, string color)
    {
	markups += ({Markup(upper_case(vertex), name, color)});
    }
    
    void add_text(string vertex, string text, string color)
    {
	markups += ({Markup(upper_case(vertex), "text:" + text, color)});
    }

    void clear_markup()
    {
	markups = ({});
    }
    
    Image.Image draw_board()
    {
	Image.Image board = Image.Image(gobansize, gobansize);
	board = board->clear(220, 150, 50);

	draw_grid(board);
	
	draw_hoshi_marks(board);

	draw_letters_and_numbers(board);

	foreach (black_stones, string stone)
	    draw_stone(board, "BLACK", stone);
	foreach (white_stones, string stone)
	    draw_stone(board, "WHITE", stone);

	markups->draw(board);
	
	return board;
    }

    static void draw_grid(Image.Image board)
    {
	int start = offset;
	int end = start + (boardsize - 1) * spacing;
	for (int k = 0; k < boardsize; k++)
	{
	    int kth = start + k * spacing;
	    board->setcolor(0, 0, 0);
	    board->line(start, kth, end, kth);
	    board->line(kth, start, kth, end);
	}
    }
    
    static void draw_hoshi_marks(Image.Image board)
    {
	int a = 2 + (boardsize >= 12);
	int b = boardsize - a - 1;
	int c = boardsize / 2;
	
	if ((boardsize % 2 == 0 && boardsize >= 8)
	    || (boardsize % 2 == 1 && boardsize >= 9))
	{
	    draw_disc(board, offset + a * spacing,
		      offset + a * spacing, 0.1 * spacing);
	    draw_disc(board, offset + a * spacing,
		      offset + b * spacing, 0.1 * spacing);
	    draw_disc(board, offset + b * spacing,
		      offset + a * spacing, 0.1 * spacing);
	    draw_disc(board, offset + b * spacing,
		      offset + b * spacing, 0.1 * spacing);
	}
	
	if (boardsize % 2 == 1 && boardsize >= 5)
	    draw_disc(board, offset + c * spacing,
		      offset + c * spacing, 0.1 * spacing);
	
	if (boardsize % 2 == 1 && boardsize >= 13)
	{
	    draw_disc(board, offset + a * spacing,
		      offset + c * spacing, 0.1 * spacing);
	    draw_disc(board, offset + b * spacing,
		      offset + c * spacing, 0.1 * spacing);
	    draw_disc(board, offset + c * spacing,
		      offset + a * spacing, 0.1 * spacing);
	    draw_disc(board, offset + c * spacing,
		      offset + b * spacing, 0.1 * spacing);
	}
	
    }
    
    static void draw_letters_and_numbers(Image.Image board)
    {
	int start = offset;
	int end = start + (boardsize - 1) * spacing;
	for (int k = 0; k < boardsize; k++)
	{
	    int kth = start + k * spacing;
	    Image.Image number = font->write((string) (boardsize - k));
	    int width = number->xsize();
	    int height = number->ysize();
	    board->paste_alpha_color(number,
				     (start - spacing / 2 - width) / 2 - 1,
				     kth - height / 2 + 1);
	    board->paste_alpha_color(number,
				     end + (start + spacing / 2 - width) / 2 + 1,
				     kth - height / 2 + 1);
	    Image.Image letter = font->write(letters[k]);
	    width = letter->xsize();
	    height = letter->ysize();
	    board->paste_alpha_color(letter,
				     kth - width / 2,
				     (start - spacing / 2 - height) / 2 - 1);
	    board->paste_alpha_color(letter,
				     kth - width / 2,
				     end + (start + spacing / 2 - height) / 2 + 1);
	}
    }

    static void draw_stone(Image.Image board, string color, string vertex)
    {
	int start = offset;
	int x, y;
	[x, y] = vertex_to_xy(upper_case(vertex));
	
	if (color == "BLACK")
	    board->setcolor(0, 0, 0);
	else if (color == "WHITE")
	    board->setcolor(255, 255, 255);
	else
	    board->setcolor(128, 128, 128);
	
	float radius = (spacing + 1) / 2.0;
	draw_disc(board, start + x * spacing, start + y * spacing, radius);
	
    }

    array(int) vertex_to_xy(string vertex)
    {
	int x = search(letters, vertex[0..0]);
	int y = boardsize - (int) vertex[1..];
	return ({x, y});
    }

    string xy_to_vertex(int x, int y)
    {
	return letters[x] + (boardsize - y);
    }

    static array(int) vertex_to_pixel_coord(string vertex)
    {
	[int x, int y] = vertex_to_xy(vertex);
	return ({offset + x * spacing, offset + y * spacing});
    }

    string pixel_coord_to_vertex(int|float pixel_x, int|float pixel_y)
    {
	int x = (int) floor((pixel_x - offset + spacing / 2.0) / spacing);
	int y = (int) floor((pixel_y - offset + spacing / 2.0) / spacing);
	if (x < 0 || x >= boardsize || y < 0 || y >= boardsize)
	    return "";
	return sprintf("%s%d", letters[x], boardsize - y);
    }
    
    static void draw_disc(Image.Image image, int|float x, int|float y,
			  int|float r, float|void alpha)
    {
	int N = 20;
	x = (float) x;
	y = (float) y;
	r = (float) r;
	if (!alpha)
	    alpha = 1.0;
	array(float) coords = ({});
	for (int k = 0; k < N; k++)
	    coords += ({r + 0.5 + r * cos(2 * 3.14159265 * k / N),
			r + 0.5 + r * sin(2 * 3.14159265 * k / N)});
	Image.Image disc = Image.Image((int) ceil(2*r), (int) ceil(2*r));
	disc->setcolor(255, 255, 255);
	disc->polyfill(coords);
	image->paste_alpha_color(disc*alpha, (int) (x - 0.5 * disc->xsize()),
				 (int) (y - 0.5 * disc->ysize()));
    }
}

string font_filename = "";

int main(int argc, array(string) argv)
{
    if (!find_font())
	return 1;
    
    SimpleGtp engine = SimpleGtp("../interface/gnugo --quiet --mode gtp -w -t -d0x101840" / " ");
    if (!engine)
    {
	werror("Failed to start engine.");
	return 1;
    }
    
    GTK.setup_gtk(argv);
    Controller controller = Controller(engine, argv[1]);
    
    return -1;

}

array(string) recursive_find_files(string dir, string suffix)
{
    array(string) found_files = ({});
    if (!get_dir(dir))
	return ({});
    foreach (get_dir(dir), string filename)
    {
	string full_name = dir + "/" + filename;
	if (Stdio.is_dir(full_name))
	    found_files += recursive_find_files(full_name, suffix);
	else if (has_suffix(filename, suffix))
	    found_files += ({full_name});
    }
    return found_files;
}

int find_font()
{
    if (getenv("GNUGO_FONT"))
    {
	font_filename = getenv("GNUGO_FONT");
	return 1;
    }

    // Search for fonts below /usr/share/fonts.
    array(string) font_files = recursive_find_files("/usr/share/fonts",
						    ".ttf");
    if (sizeof(font_files) == 0)
	font_files = recursive_find_files("/usr/share/fonts", ".pfb");

    if (sizeof(font_files) == 0)
    {
	werror("No font found while searching below /usr/share/fonts.\n");
	werror("Locate a font file with suffix .ttf (truetype) or .pfb (type1)\n");
	werror("and point to it from the environment variable GNUGO_FONT.\n");
	return 0;
    }
    
    // Choose the one with shortest name (arbitrary but may avoid e.g.
    // italic fonts).
    font_filename = font_files[0];
    foreach (font_files[1..], string font_file)
    {
	if (sizeof(font_filename) > sizeof(font_file))
	    font_filename = font_file;
	else if (sizeof(font_filename) == sizeof(font_file)
		 && has_value(lower_case(font_filename), "mono"))
	    font_filename = font_file;
    }

    return 1;
}

class RegressionViewer
{
    Goban goban;
    SimpleGtp engine;
    array(string) traces;

    string windowtitle;
    GTK.Window board_window;
    GTK.Window data_window;
    GTK.ScrolledWindow scrolled_data_window;
    array(GTK.Window) all_windows = ({});
    GTK.Image gtk_image;
    GDK.Image gdk_image;
    GTK.Clist clist;

    Controller parent; //Evil. Used for callbacks.

    mapping(string:array(string)) worms = ([]);
    mapping(string:array(string)) dragons = ([]);

    string result;
    string testcase_command;
    array(string) fulltest;

    function on_board_click_callback;

    static void create(SimpleGtp engine_, string title,
		       array(string) fulltest_, string testcase_command_,
		       function callback, Controller parent_)
    {
	engine = engine_;
	parent = parent_;
	fulltest = fulltest_;
        testcase_command = testcase_command_;

	load_testcase();
	werror("%s\n", send_command("showboard"));
	int boardsize = (int) send_command("query_boardsize");
        windowtitle = title;
	on_board_click_callback = callback;

	foreach (send_command("worm_stones") / "\n", string worm)
	    worms[(worm / " ")[0]] = worm / " " - ({""});
	
	board_window = GTK.Window(GTK.WindowToplevel);
	board_window->signal_connect("destroy", exit, 0);
	goban = Goban(boardsize, 600);
	goban->add_stones("WHITE", send_command("list_stones white") / " ");
	goban->add_stones("BLACK", send_command("list_stones black") / " ");
	Image.Image im = goban->draw_board();
	gdk_image = GDK.Image(0)->set(im);
	gtk_image = GTK.Image(gdk_image);
	board_window->add_events(GDK.ButtonPressMask);
	board_window->add_events(GDK.KeyPressMask);
	board_window->signal_connect_new("button_press_event",
					 button_pressed_on_board);
	board_window->signal_connect_new("key_press_event",
					 key_pressed_on_board);
	board_window->add(gtk_image);
	board_window->set_title(windowtitle);
	board_window->show_all();
	
	data_window = GTK.Window(GTK.WindowToplevel);
	scrolled_data_window = GTK.ScrolledWindow();
	scrolled_data_window->set_policy(GTK.POLICY_AUTOMATIC,
					 GTK.POLICY_AUTOMATIC);
	scrolled_data_window->set_usize(300,400);

	all_windows = ({board_window, data_window});
	
	clist = GTK.Clist(3);
	data_window->add(scrolled_data_window);
	scrolled_data_window->add(clist);
	data_window->set_title(windowtitle);
	data_window->show_all();
	
//	thread_create(handle_testcase);
	handle_testcase();
    }

    static void load_testcase()
    {
	foreach(fulltest, string testline) {
	    werror(testline + "\n");
	    if (!has_value("0123456789 #", testline[0..0]))
		send_command(testline);
        }
    }

    static void handle_testcase()
    {
	traces = ({});
	engine->trace_callback = collect_traces;
        result = send_command(testcase_command);
	engine->trace_callback = 0;

	// Due to a bug in the examine_position caching, we can't do
	// this before a genmove call.
	foreach (send_command("dragon_stones") / "\n", string dragon)
	    dragons[(dragon / " ")[0]] = dragon / " " - ({""});

	redraw_board();
    }

    static void collect_traces(string s)
    {
	traces += ({s});
    }
    
    void add_markup(int mode)
    {
	goban->clear_markup();
	if (mode <= 4) {
	    function add_suitable_markup = ({add_worms_and_dragons_markup,
					     add_move_generation_markup,
					     add_eyes_markup,
					     add_influence_markup,
					     add_reading_markup})[mode];
	    add_suitable_markup();
	    redraw_board();
	}
    }
    
    static void add_worms_and_dragons_markup()
    {
	mapping status_colors = (["alive":"green",
				  "critical":"yellow",
				  "dead":"red",
				  "unknown":"blue"]);
	mapping safety_colors = (["alive":"green",
				  "critical":"yellow",
				  "dead":"red",
				  "tactically dead":"brown",
				  "alive in seki":"cyan",
				  "strongly alive":"blue",
				  "invincible":"purple",
				  "inessential":"orange"]);
	
	if (parent->dragon_status_button->get_active())
	{
	    foreach (dragons; string dragon; array(string) stones)
	    {
		string status = get_worm_or_dragon_data("dragon", "status",
							dragon);
		foreach (stones, string stone)
		    goban->add_symbol(stone, "dot",
				      status_colors[status]);
	    }
	}
	else if (parent->dragon_safety_button->get_active())
	{
	    foreach (dragons; string dragon; array(string) stones)
	    {
		string safety = get_worm_or_dragon_data("dragon", "safety",
							dragon);
		foreach (stones, string stone)
		    goban->add_symbol(stone, "dot",
				      safety_colors[safety]);
	    }
	}
	else if (parent->worm_status_button->get_active())
	{
	    foreach (worms; string worm; array(string) stones)
	    {
		string attack = get_worm_or_dragon_data("worm", "attack_code",
							worm);
		string defense = get_worm_or_dragon_data("worm",
							 "defense_code", worm);
		string status = "alive";
		if (attack != "0")
		{
		    if (defense == "0")
			status = "dead";
		    else
			status = "critical";
		}
		
		foreach (stones, string stone)
		    goban->add_symbol(stone, "dot",
				      status_colors[status]);
	    }
	}
    }
    
    static void add_move_generation_markup()
    {
	if ((parent->top_moves_button->get_active()
	     || parent->all_moves_button->get_active())
	    && (has_prefix(testcase_command, "reg_genmove")
		|| has_prefix(testcase_command, "restricted_genmove")))
	{
	    string color = "green";
	    string answers = parent->expected_result;
	    if (answers[0..0] == "!") {
		answers = answers[1..];
		color = "red";
	    }
	    if (has_prefix(testcase_command, "restricted_genmove"))
	    {
		foreach ((testcase_command / " ")[2..], string allowed_move)
		{
		    if (color == "green" ^ has_value(answers, allowed_move))
			goban->add_symbol(allowed_move, "triangle", "red");
		    else
			goban->add_symbol(allowed_move, "triangle", "green");
		}
	    }
	    else
		foreach (answers / "|" - ({""}), string answer)
		    goban->add_symbol(answer, "big_square", color);

	    
	    color = (testcase_command / " ")[1];
	    goban->add_symbol(result, "stone", color);
	}

	if (parent->top_moves_button->get_active())
	{
	    array(string) top_moves = send_command("top_moves") / " ";
	    for (int k = 0; k < sizeof(top_moves) / 2; k++)
		goban->add_text(top_moves[2 * k],
				top_moves[2 * k + 1], "blue");
	}
	else if (parent->all_moves_button->get_active())
	{
	    array(string) all_moves = send_command("all_move_values") / "\n";
	    foreach (all_moves, string move_value)
	    {
		sscanf(move_value, "%s%*[ ]%s", string vertex, string value);
		goban->add_text(vertex, value, "blue");
	    }	    
	}
	else if (parent->delta_territory_button->get_active()
		 && parent->delta_territory_move != "PASS")
	{
	    goban->add_symbol(parent->delta_territory_move, "stone", "gray");
	    parent->delta_territory_button_text
	    		->set_text("delta territory for "
				   + parent->delta_territory_move);
	    clist->clear();

	    int k;
	    for (k = sizeof(traces) - 1; k >= 0; k--)
	    {
		if (sscanf(traces[k], "  " + parent->delta_territory_move
			   + ": %*f - change in territory%*s") == 2
		    && !has_value(traces[k], "cached"))
		    break;
	    }
	    if (k >= 0)
	    {
		clist->append(({traces[k], "", ""}));
		for (k--; k >= 0; k--)
		{
		    if (sscanf(traces[k], "    %*s:   - %*s") < 2)
			break;

		    clist->prepend(({traces[k], "", ""}));
		    if (sscanf(traces[k],
			       "    %*s:   - %s territory change %s ",
			       string vertex, string value) == 3)
		    {
			goban->add_text(vertex, value,
					(float) value < 0.0 ? "red" : "blue");
		    }
		}
	    }
	    clist->columns_autosize();
	    data_window->set_title("Delta territory for "
				   + parent->delta_territory_move);
	}
    }

    static mapping(string:mapping(string:string)) eye_data = 0;
    static mapping(string:string) half_eye_data = ([]);
    static mapping(string:mapping(string:string)) eye_types = 0;
    
    static void add_eyes_markup()
    {
	if (!eye_data)
	{
	    eye_data = (["white":([]), "black":([])]);
	    eye_types = (["white":([]), "black":([])]);
	    for (int y = 0; y < goban->boardsize; y++)
	    {
		for (int x = 0; x < goban->boardsize; x++)
		{
		    string vertex = goban->xy_to_vertex(x, y);
		    multiset(string) is_marginal = (<>);
		    foreach (({"white", "black"}), string color)
		    {
			string this_eye = send_command("eye_data " + color +
						       " " + vertex);
			if (!Regexp("origin +PASS")->match(this_eye))
			{
			    eye_data[color][vertex] = this_eye;
			    if (Regexp("marginal +1")->match(this_eye))
				is_marginal[color] = 1;
			}
		    }

		    if (!eye_data["white"][vertex]
			&& !eye_data["black"][vertex])
			continue;
		    
		    string half_eye = send_command("half_eye_data " + vertex);
		    if (!Regexp("type +0")->match(half_eye))
			half_eye_data[vertex] = half_eye;

		    foreach (({"white", "black"}), string color)
		    {
			if (!eye_data[color][vertex])
			    continue;
			if (is_marginal[color])
			    eye_types[color][vertex] = "marginal";
			else if (Regexp("type +HALF_EYE")->match(half_eye))
			    eye_types[color][vertex] = "half";
			else
			    eye_types[color][vertex] = "proper";
		    }
		}
	    }
	}

	string color;
	if (parent->white_eyes_button->get_active())
	    color = "white";
	else
	    color = "black";

	foreach (eye_types[color]; string vertex; string eye_type)
	{
	    mapping (string:string) grayish = (["white" : "#c0c0c0",
						"black" : "#404040"]);
	    if (eye_type == "proper")
		goban->add_symbol(vertex, "big_dot", color);
	    else if (eye_type == "half")
		goban->add_symbol(vertex, "big_dot", grayish[color]);
	    else
		goban->add_symbol(vertex, "dot", grayish[color]);
	}
    }
    
    static void add_influence_markup()
    {
	string command;
	string what_data;
	if (parent->initial_w_influence_dragons_known_button->get_active())
	    command = "initial_influence white";
	else if (parent->initial_b_influence_dragons_known_button->get_active())
	    command = "initial_influence black";
	else if (parent->after_move_influence_button->get_active())
	{
	    if (parent->move_influence_move == "PASS")
		return;
	    command = sprintf("move_influence %s %s",
	    		      parent->current_move_color,
			      parent->move_influence_move);
	    goban->add_symbol(parent->move_influence_move, "stone", "gray");
	}
	else if (parent->followup_influence_button->get_active())
	{
	    if (parent->move_influence_move == "PASS")
		return;
	    command = sprintf("followup_influence %s %s",
	    		      parent->current_move_color,
			      parent->move_influence_move);
	    goban->add_symbol(parent->move_influence_move, "stone", "gray");
	}

	if (parent->influence_regions_button->get_active())
	    what_data = "influence_regions";
	if (parent->territory_value_button->get_active())
	    what_data = "territory_value";
	else if (parent->white_influence_button->get_active())
	    what_data = "white_influence";
	else if (parent->black_influence_button->get_active())
	    what_data = "black_influence";
	else if (parent->white_influence_button->get_active())
	    what_data = "white_influence";
	else if (parent->black_influence_button->get_active())
	    what_data = "black_influence";
	else if (parent->white_strength_button->get_active())
	    what_data = "white_strength";
	else if (parent->black_strength_button->get_active())
	    what_data = "black_strength";
	else if (parent->white_permeability_button->get_active())
	    what_data = "white_permeability";
	else if (parent->black_permeability_button->get_active())
	    what_data = "black_permeability";
	else if (parent->white_attenuation_button->get_active())
	    what_data = "white_attenuation";
	else if (parent->black_attenuation_button->get_active())
	    what_data = "black_attenuation";
	else if (parent->non_territory_button->get_active())
	    what_data = "non_territory";

	string result = send_command(command + " " + what_data);
	array(string) values = (replace(result, "\n", " ") / " ") - ({""});
	int k = 0;
	for (int y = 0; y < goban->boardsize; y++)
	{
	    for (int x = 0; x < goban->boardsize; x++)
	    {
		string vertex = goban->xy_to_vertex(x, y);
		if (what_data == "influence_regions")
		{
		    int value = (int) values[k];
		    mapping(int:string) sizes = ([3:"big_dot",
						  2:"big_dot",
						  1:"dot",
						  -1:"dot",
						  -2:"big_dot",
						  -3:"big_dot"]);
		    mapping(int:string) colors = ([3:"white",
						   2:"#c0c0c0",
						   1:"#c0c0c0",
						   -1:"#404040",
						   -2:"#404040",
						   -3:"black"]);
		    if (sizes[value])
			goban->add_symbol(vertex, sizes[value], colors[value]);
		}
		else if (has_value(what_data, "permeability"))
		{
		    if ((float) values[k] != 1.0)
			goban->add_text(vertex, values[k], "blue");
		}
		else if (what_data == "non_territory")
		{
		    if (!goban->occupied(vertex))
		    {
			int value = (int) values[k];
			if (value == 1)
			    goban->add_symbol(vertex, "dot", "black");
			else if (value == 2)
			    goban->add_symbol(vertex, "dot", "white");
			else if (value == 0)
			    goban->add_symbol(vertex, "dot", "gray");
		    }
		}
		else if ((float) values[k] > 0.0)
		    goban->add_text(vertex, values[k], "blue");
		else if ((float) values[k] < 0.0)
		    goban->add_text(vertex, values[k], "red");

		k++;
	    }
	}
    }
    
    static void add_reading_markup()
    {
	if ((parent->connection_reading_button->get_active()
	     || parent->semeai_reading_button->get_active())
	    && parent->first_semeai_or_connection_vertex != "")
	    goban->add_symbol(parent->first_semeai_or_connection_vertex,
	    		      "big_dot", "green");
    }
    
    void redraw_board()
    {
	gdk_image->set(goban->draw_board());
	gtk_image->queue_draw();
    }

    void show_worm_data(string vertex)
    {
	string worm_data = send_command("worm_data " + vertex);
	clist->clear();
	foreach ((worm_data / "\n")[1..], string data_item)
	{
	    sscanf(data_item, "%s%*[ ]%s", string field, string value);
	    clist->append(({field, value, ""}));
	}
	clist->columns_autosize();
	data_window->set_title("Worm data for " + vertex);
    }

    void show_dragon_data(string vertex, int part)
    {
	string dragon_data = send_command("dragon_data " + vertex);
	clist->clear();
	
	array(string) selected_data;
	if (part == 1)
	    selected_data = (dragon_data / "\n")[1..20];
	else
	    selected_data = (dragon_data / "\n")[21..];
	
	foreach (selected_data, string data_item)
	{
	    sscanf(data_item, "%s%*[ ]%s", string field, string value);
	    clist->append(({field, value, ""}));
	}
	clist->columns_autosize();
	data_window->set_title("Dragon data for " + vertex);
    }

    void show_move_reasons(string vertex)
    {
	string move_reasons = send_command("move_reasons " + vertex);
	clist->clear();

	foreach ((move_reasons / "\n"), string move_reason)
	    clist->append(({move_reason, "", ""}));

	int k;
	for (k = sizeof(traces) - 1; k >= 0; k--)
	{
	    if (sscanf(traces[k], "Move generation values "
		       + vertex + " to %*s") == 1)
		break;
	}
	if (k >= 0)
	{
	    array(string) interesting_lines = ({traces[k]});
	    for (k--; k >= 0; k--)
	    {
		if (sscanf(traces[k], "  " + vertex + ": %*s") == 1)
		    interesting_lines += ({traces[k]});
		else if (sscanf(traces[k], "    " + vertex + ": %*s") != 1)
		    break;
	    }
	    clist->append(({"", "", ""}));
	    foreach (reverse(interesting_lines), string line)
		clist->append(({line, "", ""}));
	}

	int first_pattern = 1;
	int add_continuation_lines = 0;
	foreach (traces, string line)
	{
	    if (sscanf(line, "pattern %*s matched at " + vertex + "%s",
		       string s) == 2
		&& s == "")
	    {
		if (first_pattern)
		{
		    clist->append(({"", "", ""}));
		    first_pattern = 0;
		}
		add_continuation_lines = 1;
		clist->append(({line, "", ""}));
	    }
	    else if (has_prefix(line, "...") && add_continuation_lines)
		clist->append(({line, "", ""}));
	    else
		add_continuation_lines = 0;
	}

	clist->columns_autosize();
	data_window->set_title("Move reasons for " + vertex);
    }
    
    void show_eye_data(string vertex)
    {
	clist->clear();

	string color;
	if (parent->white_eyes_button->get_active())
	    color = "white";
	else
	    color = "black";

	if (eye_data[color][vertex])
	{
	    foreach (eye_data[color][vertex] / "\n", string data_item)
	    {
		sscanf(data_item, "%s%*[ ]%s", string field, string value);
		clist->append(({field, value, ""}));
	    }
	    if (half_eye_data[vertex])
	    {
		clist->append(({"", "", ""}));
		foreach (half_eye_data[vertex] / "\n", string data_item)
		{
		    sscanf(data_item, "%s%*[ ]%s", string field, string value);
		    clist->append(({field, value, ""}));
		}
	    }
	}
	
	clist->columns_autosize();
	data_window->set_title(color + " eye data for " + vertex);
    }

   
    static void button_pressed_on_board(GDK.Event event)
    {
//	werror("Button: %O\n", (mapping) event);
	string vertex = goban->pixel_coord_to_vertex(event->x, event->y);
        on_board_click_callback(vertex);
    }

    static void key_pressed_on_board(GDK.Event event)
    {
//	werror("Key: %O\n", (mapping) event);
	// First thing to find out is what the event object contains and
	// how we can get the mouse position when the key was pressed.
    }
    
    string send_command(string command)
    {
	string result;
	all_windows->set_cursor(GDK.Watch);
	result = engine->send_command(command)->text;
	all_windows->set_cursor(GDK.TopLeftArrow);
	return result;
    }

    mapping(string:string) worm_and_dragon_cache = ([]);

    static string get_worm_or_dragon_data(string worm_or_dragon, string field,
					  string vertex)
    {
	string command = worm_or_dragon + "_data " + vertex;
	string data = worm_and_dragon_cache[command];
	if (!data)
	{
	    data = send_command(command);
	    worm_and_dragon_cache[command] = data;
	}
	
	foreach (data / "\n", string row)
	    if (has_prefix(row, field))
	    {
		sscanf(row, "%*s%*[ ]%s", string value);
		return value;
	    }
	
	return "";
    }

    void do_reading(string reset_counter, string get_counter,
	    	    string sgffilename,
    		    string first_command, string second_command)
    {
	string result;
	send_command("clear_cache");
	if (sgffilename != "")
	    send_command("start_sgftrace");

	send_command(reset_counter);
	result = send_command(first_command);
	clist->append(({first_command, result,
			"(" + send_command(get_counter) + " nodes)"}));
	
	if (result[0..0] != "0")
	{
	    send_command(reset_counter);
	    string result = send_command(second_command);
	    clist->append(({second_command, result,
			    "(" + send_command(get_counter) + " nodes)"}));
	}
	if (sgffilename != "")
	    send_command("finish_sgftrace " + sgffilename);
	clist->columns_autosize();
	data_window->set_title("Reading result");
     }
}


class Controller
{
    array(RegressionViewer) viewers = ({});

    string current_move_color = "";
    
    GTK.Window notebook_window;
    GTK.Notebook notebook;
    
    GTK.RadioButton worm_data_button;
    GTK.RadioButton dragon_data1_button;
    GTK.RadioButton dragon_data2_button;
    GTK.RadioButton worm_status_button;
    GTK.RadioButton dragon_status_button;
    GTK.RadioButton dragon_safety_button;
    
    GTK.RadioButton top_moves_button;
    GTK.RadioButton all_moves_button;
    GTK.RadioButton delta_territory_button;
    GTK.Label delta_territory_button_text;
    
//    GTK.RadioButton initial_w_influence_dragons_unknown_button;
//    GTK.RadioButton initial_b_influence_dragons_unknown_button;
    GTK.RadioButton initial_w_influence_dragons_known_button;
    GTK.RadioButton initial_b_influence_dragons_known_button;
    GTK.RadioButton after_move_influence_button;
    GTK.Label after_move_influence_button_text;
    GTK.RadioButton followup_influence_button;
    GTK.Label followup_influence_button_text;
    GTK.RadioButton influence_regions_button;
    GTK.RadioButton territory_value_button;
    GTK.RadioButton white_influence_button;
    GTK.RadioButton black_influence_button;
    GTK.RadioButton white_strength_button;
    GTK.RadioButton black_strength_button;
    GTK.RadioButton white_permeability_button;
    GTK.RadioButton black_permeability_button;
    GTK.RadioButton white_attenuation_button;
    GTK.RadioButton black_attenuation_button;
    GTK.RadioButton non_territory_button;

    GTK.RadioButton white_eyes_button;
    GTK.RadioButton black_eyes_button;
    
    GTK.RadioButton tactical_reading_button;
    GTK.RadioButton owl_reading_button;
    GTK.RadioButton connection_reading_button;
    GTK.RadioButton semeai_reading_button;
    GTK.CheckButton sgf_traces_button;
    GTK.Entry sgf_filename_entry;
    GTK.Button new_engine_button;
    GTK.Entry engine_path_entry;
    
    string delta_territory_move = "PASS";
    string move_influence_move = "PASS";
    string first_semeai_or_connection_vertex = "";

    string testcase_name;
    string testcase_command;
    string result;
    string expected_result;

    // All lines from the test file shown at the top of the control window.
    array(string) full_testcase;

    // All lines from the test file which may be needed to load the
    // testcase correctly.
    array(string) complete_testcase;

    static void create(SimpleGtp engine_, string testcase)
    {
	if (!excerpt_testcase(testcase, engine_))
	{
	    werror("Failed to load testcase.\n");
	    exit(1);
	}
	testcase_name = testcase;

        viewers += ({RegressionViewer(engine_, testcase, complete_testcase,
				      testcase_command,
				      button_pressed_on_a_board,
				      this_object())});

	notebook_window = GTK.Window(GTK.WindowToplevel);
	notebook = GTK.Notebook();
	notebook->set_tab_pos(GTK.POS_LEFT);
	notebook_window->add(GTK.Vbox(0, 0)
			     ->add(GTK.Label(full_testcase * "\n")
				   ->set_justify(GTK.JUSTIFY_LEFT))
			     ->add(notebook));
	notebook_window->set_title(testcase);
	notebook_window->signal_connect("destroy", exit, 0);
	
	if (has_prefix(testcase_command, "reg_genmove")
	    || has_prefix(testcase_command, "restricted_genmove"))
	{
//	    notebook->set_page(1);
	    sscanf(testcase_command, "%*s %s", string color);
	    if (lower_case(color[0..0]) == "w")
		current_move_color = "white";
	    else
		current_move_color = "black";
	}

	worm_data_button = GTK.RadioButton("worm data");
	dragon_data1_button = GTK.RadioButton("dragon data, part 1",
					      worm_data_button);
	dragon_data2_button = GTK.RadioButton("dragon data, part 2",
					      worm_data_button);

	worm_status_button = GTK.RadioButton("worm status");
	dragon_status_button = GTK.RadioButton("dragon status",
					       worm_status_button);
	dragon_safety_button = GTK.RadioButton("dragon safety",
					       worm_status_button);
	worm_status_button->signal_connect_new("clicked",
					       markup_button_pressed);
	dragon_status_button->signal_connect_new("clicked",
						 markup_button_pressed);
	dragon_safety_button->signal_connect_new("clicked",
						 markup_button_pressed);

	top_moves_button = GTK.RadioButton("top moves");
	all_moves_button = GTK.RadioButton("all moves", top_moves_button);
	delta_territory_button_text = GTK.Label("delta territory for PASS");
	delta_territory_button_text->set_justify(GTK.JUSTIFY_LEFT);
	delta_territory_button = GTK.RadioButton(0, top_moves_button);
	delta_territory_button->add(delta_territory_button_text);
	top_moves_button->signal_connect_new("clicked", markup_button_pressed);
	all_moves_button->signal_connect_new("clicked", markup_button_pressed);
	delta_territory_button->signal_connect_new("clicked",
						   markup_button_pressed);

	white_eyes_button = GTK.RadioButton("white eyes");
	black_eyes_button = GTK.RadioButton("black_eyes", white_eyes_button);
	white_eyes_button->signal_connect_new("clicked",
					      markup_button_pressed);
	black_eyes_button->signal_connect_new("clicked",
					      markup_button_pressed);
	
//	  initial_w_influence_dragons_unknown_button =
//	      GTK.RadioButton("white influence, dragons unknown");
//	  initial_b_influence_dragons_unknown_button =
//	      GTK.RadioButton("black influence, dragons unknown",
//			      initial_w_influence_dragons_unknown_button);
	initial_w_influence_dragons_known_button =
	    GTK.RadioButton("white influence, dragons known");
	initial_b_influence_dragons_known_button =
	    GTK.RadioButton("black influence, dragons known",
			    initial_w_influence_dragons_known_button);
	after_move_influence_button_text =
	    GTK.Label("after move influence for PASS");
	after_move_influence_button_text->set_justify(GTK.JUSTIFY_LEFT);
	after_move_influence_button =
	    GTK.RadioButton(0, initial_w_influence_dragons_known_button);
	after_move_influence_button->add(after_move_influence_button_text);
	followup_influence_button_text =
	    GTK.Label("followup influence for PASS");
	followup_influence_button_text->set_justify(GTK.JUSTIFY_LEFT);
	followup_influence_button =
	    GTK.RadioButton(0, initial_w_influence_dragons_known_button);
	followup_influence_button->add(followup_influence_button_text);
	influence_regions_button = GTK.RadioButton("influence regions");
	territory_value_button = GTK.RadioButton("territory value",
						 influence_regions_button);
	white_influence_button = GTK.RadioButton("white influence",
						 influence_regions_button);
	black_influence_button = GTK.RadioButton("black influence",
						 influence_regions_button);
	white_strength_button = GTK.RadioButton("white strength",
						influence_regions_button);
	black_strength_button = GTK.RadioButton("black strength",
						influence_regions_button);
	white_permeability_button = GTK.RadioButton("white permeability",
						    influence_regions_button);
	black_permeability_button = GTK.RadioButton("black permeability",
						    influence_regions_button);
	white_attenuation_button = GTK.RadioButton("white attenuation",
						   influence_regions_button);
	black_attenuation_button = GTK.RadioButton("black attenuation",
						   influence_regions_button);
	non_territory_button = GTK.RadioButton("non-territory",
					       influence_regions_button);
	({initial_w_influence_dragons_known_button,
	  initial_b_influence_dragons_known_button,
	  after_move_influence_button,
	  followup_influence_button,
	  influence_regions_button,
	  territory_value_button,
	  white_influence_button,
	  black_influence_button,
	  white_strength_button,
	  black_strength_button,
	  white_permeability_button,
	  black_permeability_button,
	  white_attenuation_button,
	  black_attenuation_button,
	  non_territory_button})->signal_connect_new("clicked",
						     markup_button_pressed);
	
	
	tactical_reading_button = GTK.RadioButton("tactical reading");
	owl_reading_button = GTK.RadioButton("owl reading",
					     tactical_reading_button);
	connection_reading_button = GTK.RadioButton("connection reading",
						    tactical_reading_button);
	semeai_reading_button = GTK.RadioButton("semeai reading",
						tactical_reading_button);
	sgf_traces_button = GTK.CheckButton("save sgf traces to");
	sgf_filename_entry = GTK.Entry();
	sgf_filename_entry->set_text("vars.sgf");
	sgf_filename_entry->set_editable(1);

	engine_path_entry = GTK.Entry();
	engine_path_entry->set_text("../interface/gnugo");
	engine_path_entry->set_editable(1);
	new_engine_button = GTK.Button("Start new engine");
	new_engine_button->signal_connect_new("clicked", select_new_engine);
	
	notebook->append_page(GTK.Vbox(0, 0)
			      ->pack_start(worm_data_button, 0, 0, 0)
			      ->pack_start(dragon_data1_button, 0, 0, 0)
			      ->pack_start(dragon_data2_button, 0, 0, 0)
			      ->pack_start(GTK.Label(""), 0, 0, 0)
			      ->pack_start(worm_status_button, 0, 0, 0)
			      ->pack_start(dragon_status_button, 0, 0, 0)
			      ->pack_start(dragon_safety_button, 0, 0, 0),
			      GTK.Label("worms and dragons"));
	notebook->append_page(GTK.Vbox(0, 0)
			      ->pack_start(top_moves_button, 0, 0, 0)
			      ->pack_start(all_moves_button, 0, 0, 0)
			      ->pack_start(delta_territory_button, 0, 0, 0),
			      GTK.Label("move generation"));
	notebook->append_page(GTK.Vbox(0, 0)
			      ->pack_start(white_eyes_button, 0, 0, 0)
			      ->pack_start(black_eyes_button, 0, 0, 0),
			      GTK.Label("eyes"));
	notebook->append_page(GTK.Vbox(0, 0)
//			      ->add(initial_w_influence_dragons_unknown_button)
//			      ->add(initial_b_influence_dragons_unknown_button)
			      ->add(initial_w_influence_dragons_known_button)
			      ->add(initial_b_influence_dragons_known_button)
			      ->add(after_move_influence_button)
			      ->add(followup_influence_button)
			      ->add(GTK.Label(""))
			      ->add(influence_regions_button)
			      ->add(territory_value_button)
			      ->add(non_territory_button)
			      ->add(white_influence_button)
			      ->add(black_influence_button)
			      ->add(white_strength_button)
			      ->add(black_strength_button)
			      ->add(white_permeability_button)
			      ->add(black_permeability_button)
			      ->add(white_attenuation_button)
			      ->add(black_attenuation_button),
			      GTK.Label("influence"));
	notebook->append_page(GTK.Vbox(0, 0)
			      ->pack_start(tactical_reading_button, 0, 0, 0)
			      ->pack_start(owl_reading_button, 0, 0, 0)
			      ->pack_start(connection_reading_button, 0, 0, 0)
			      ->pack_start(semeai_reading_button, 0, 0, 0)
			      ->pack_start(GTK.Label(""), 0, 0, 0)
			      ->pack_start(GTK.Hbox(0, 0)
					   ->pack_start(sgf_traces_button,
							0, 0, 0)
					   ->pack_start(sgf_filename_entry,
							0, 0, 0), 0, 0, 0),
			      GTK.Label("reading"));
	notebook->append_page(GTK.Vbox(0, 0)
			      ->pack_start(engine_path_entry, 0, 0, 0)
			      ->pack_start(new_engine_button, 0, 0, 0),
			      GTK.Label("Engines"));
	notebook->signal_connect_new("switch_page", add_markup);

	if (has_prefix(testcase_command, "reg_genmove")
	    || has_prefix(testcase_command, "restricted_genmove"))
	    notebook->set_page(1);

	notebook_window->show_all();
    }

    static void select_new_engine()
    {
	string new_engine_path = engine_path_entry->get_text();
	SimpleGtp new_engine = SimpleGtp((new_engine_path + " --quiet --mode gtp -w -t -d0x101840") / " ");
    	if (!new_engine)
	    werror("Failed to start new engine.\n");
	else
	    viewers += ({RegressionViewer(new_engine,
					  testcase_name, full_testcase,
					  testcase_command,
					  button_pressed_on_a_board,
					  this_object())});
    }

    static void markup_button_pressed(mixed ... foo)
    {
	add_markup(notebook->get_current_page());
    }

    static void add_markup(int mode)
    {
	viewers->add_markup(mode);
    }
    
    void button_pressed_on_a_board(string vertex)
    {
	if (vertex == "")
	    return;
	if (notebook->get_current_page() == 0)
	{
	    // Worms and dragons.
	    if (worm_data_button->get_active())
		viewers->show_worm_data(vertex);
	    else if (dragon_data1_button->get_active())
		viewers->show_dragon_data(vertex, 1);
	    else
		viewers->show_dragon_data(vertex, 2);
	}
	else if (notebook->get_current_page() == 1)
	{
	    // Move generation.
	    if (!delta_territory_button->get_active())
		viewers->show_move_reasons(vertex);
	    else
	    {
		delta_territory_move = vertex;
		markup_button_pressed();
	    }
	}
	else if (notebook->get_current_page() == 2)
	{
	    // Eyes.
	    viewers->show_eye_data(vertex);
	}
	else if (notebook->get_current_page() == 3)
	{
	    // Influence.
	    if (after_move_influence_button->get_active()
		|| followup_influence_button->get_active())
	    {
		move_influence_move = vertex;
		after_move_influence_button_text
		    ->set_text("after move influence for " + vertex);
		followup_influence_button_text
		    ->set_text("followup influence for " + vertex);
		markup_button_pressed();
	    }
	}
	else if (notebook->get_current_page() == 4)
	{
	    // Reading.
	    string sgffilename;
	    string reset_counter, get_counter;
	    
	    if (sgf_traces_button->get_active())
	    	sgffilename = sgf_filename_entry->get_text();
	    else
	    	sgffilename = "";

	    if (first_semeai_or_connection_vertex == ""
		|| tactical_reading_button->get_active()
		|| owl_reading_button->get_active())
		viewers->goban->clear_markup();
		
	    viewers->goban->add_symbol(vertex, "big_dot", "green");
	    viewers->redraw_board();

	    viewers->clist->clear();

	    if (tactical_reading_button->get_active()
		|| owl_reading_button->get_active())
	    {
		string prefix = "";
		reset_counter = "reset_reading_node_counter";
		get_counter = "get_reading_node_counter";
		if (owl_reading_button->get_active())
		{
		    prefix = "owl_";
		    reset_counter = "reset_owl_node_counter";
		    get_counter = "get_owl_node_counter";
		}
		
		viewers->do_reading(reset_counter, get_counter, sgffilename,
				    prefix + "attack " + vertex,
				    prefix + "defend " + vertex);
	    }
	    else if (connection_reading_button->get_active()
		     || semeai_reading_button->get_active())
	    {
		if (first_semeai_or_connection_vertex == "")
		{
		    first_semeai_or_connection_vertex = vertex;
		}
		else if (first_semeai_or_connection_vertex != vertex)
		{
		    string c1, c2;
		    
		    if (connection_reading_button->get_active())
		    {
			c1 = sprintf("connect %s %s\n",
				    first_semeai_or_connection_vertex,
				    vertex);
			c2 = "dis" + c1;
			viewers->do_reading("reset_connection_node_counter",
					    "get_connection_node_counter",
					    sgffilename, c1, c2);
		    }
		    else
		    {
			c1 = sprintf("analyze_semeai %s %s",
				     first_semeai_or_connection_vertex,
				     vertex);
			c2 = sprintf("analyze_semeai %s %s", vertex,
				     first_semeai_or_connection_vertex);
			// FIXME: We should use a semeai node counter rather
			// than the owl node counter, except that it doesn't
			// exist yet.
			viewers->do_reading("reset_owl_node_counter",
					    "get_owl_node_counter",
					    sgffilename, c1, c2);
		    }
		    first_semeai_or_connection_vertex = "";
		}
	    }
	}
    }

    // The engine parameter is only needed to find out the color to
    // move when an sgf file is given. Since there can be multiple
    // viewers we shouldn't use this engine object for anything else
    // though. Notice also that no viewer has been set up yet at the
    // time of this call.
    static int excerpt_testcase(string testcase, SimpleGtp engine)
    {
	string filename;
	int number;
	if (sscanf(testcase, "%s:%d", filename, number) < 2)
	    return 0;
	
	if (!has_suffix(filename, ".tst")
	    && !has_suffix(filename, ".sgf"))
	    filename += ".tst";
	
	string testfile = Stdio.read_file(filename);
	if (!testfile)
	    return 0;

	if (has_suffix(filename, ".sgf"))
	{
	    // Only sgf file provided. Fake a testcase.
	    string s = "loadsgf " + filename + " " + number;
	    string color = engine->send_command(s)->text;
	    testcase_command = "reg_genmove " + color;
	    full_testcase = ({s, testcase_command});
	    complete_testcase = ({s, testcase_command});
	    expected_result = "";
	    return 1;
	}
	
	full_testcase = ({});
	complete_testcase = ({});
	array(string) testlines = testfile / "\n";
	for (int k = 0; k < sizeof(testlines); k++)
	{
	    int this_number;
	    string testline = testlines[k];
	    if (testline[0..0] >= "a" && testline[0..0] <= "z") {
	        if (testline[0..6] != "loadsgf")
		    complete_testcase += ({ testline });
		else
		    complete_testcase = ({ testline });
	    }
	    else if (sscanf(testline, "%d %s", this_number, testline) == 2
		&& this_number == number)
	    {
		testcase_command = testline;
		sscanf(testlines[k + 1], "#? [%s]", expected_result);
		full_testcase += ({testlines[k]});
		full_testcase += ({testlines[k + 1]});
		return 1;
	    }

	    if (has_value("0123456789 ", testline[0..0]))
		full_testcase = ({});
	    else
		full_testcase += ({testline});
	}
	
	return 0;
    }

    
    void debug_callback(mixed ... args)
    {
	write("Debug callback:%O\n", args);
    }
}
