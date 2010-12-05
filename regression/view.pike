#!/usr/bin/env pike

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This distributed with GNU Go, a go program.			     * 
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3             *
 * or (at your option) any later version.                            *
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

// Defaults:
// linux
    string sgf_viewer_command = "quarry %s";                                 
// windows (for example)
//  string sgf_viewer_command = "c:\\programs\\winmgt\\winmgt %s";     

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

    string command_line;
    
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

	    s -= "\r";
	    
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
	    s -= "\r";
	    if (trace_callback)
		trace_callback(s);
	}
	
	engine_err->close();
    }
    
    void create(array(string) program_start_array,
		function|void crash_callback_)
    {
	command_line = program_start_array * " ";
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
    if (argc < 2) {
	werror("Usage: %s TEST-FILE:TEST-NUMBER\n", basename(argv[0]));
	return 1;
    }

    if (!find_font())
	return 1;

    SimpleGtp engine = SimpleGtp("../interface/gnugo --quiet --mode gtp -w -t -d0x101840" / " ");
    if (!engine)
    {
	werror("Failed to start engine.");
	return 1;
    }

    GTK2.setup_gtk(argv);
    Controller controller = Controller(engine, argv[1..]);
    GTK2.main();

    return 0;
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

    // Compute the length of the filename proper, i.e. without the
    // path to the file.
    int fontlength(string s) {
	return sizeof((s / "/")[-1]);
    };
    
    // Choose the one with shortest name (arbitrary but may avoid e.g.
    // italic fonts).
    font_filename = font_files[0];
    foreach (font_files[1..], string font_file)
    {
	if (fontlength(font_filename) > fontlength(font_file))
	    font_filename = font_file;
	else if (fontlength(font_filename) == fontlength(font_file)
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

    GTK2.Widget goban_widget;
    GTK2.Widget data_widget;

    GTK2.ScrolledWindow scrolled_data_window;
    GTK2.Image gtk_image;
    GTK2.GdkImage gdk_image;
    GTK2.TextView clistview;
    GTK2.TextBuffer clist;

    Controller parent; //Evil. Used for callbacks.

    mapping(string:array(string)) worms = ([]);
    mapping(string:array(string)) dragons = ([]);
    int worms_initialized = 0;
    int dragons_initialized = 0;

    string name;
    string result;
    string testcase_command;
    array(string) complete_test;

    function on_board_click_callback;

    static void create(SimpleGtp engine_,
		       array(string) complete_test_, string testcase_command_,
		       function callback, string name_,
		       Controller parent_)
    {
	engine = engine_;
	parent = parent_;
	complete_test = complete_test_;
        testcase_command = testcase_command_;
	name = name_;

	load_testcase();
	werror("%s\n", send_command("showboard"));
	int boardsize = (int) send_command("query_boardsize");
	on_board_click_callback = callback;

	setup_board(boardsize);

	scrolled_data_window = GTK2.ScrolledWindow();
	scrolled_data_window->set_policy(GTK2.POLICY_AUTOMATIC,
					 GTK2.POLICY_AUTOMATIC);

	clist = GTK2.TextBuffer();
	clistview = GTK2.TextView(clist);
	clistview->set_editable(0);
	clistview->set_cursor_visible(0);
	clistview->modify_font(GTK2.PangoFontDescription("Monospace 7"));
	scrolled_data_window->add(clistview);
	handle_testcase();
    }

    static void setup_board(int boardsize)
    {
	goban = Goban(boardsize, 600);
	goban->add_stones("WHITE", send_command("list_stones white") / " ");
	goban->add_stones("BLACK", send_command("list_stones black") / " ");
	Image.Image im = goban->draw_board();

	gdk_image = GTK2.GdkImage(0)->set(im);
	gtk_image = GTK2.Image(gdk_image);
	goban_widget = GTK2.EventBox()->add(gtk_image);
	goban_widget->add_events(GTK2.GdkButtonPressMask);
	goban_widget->add_events(GTK2.GdkKeyPressMask);
	goban_widget->signal_connect("button_press_event",
				     button_pressed_on_board);
	goban_widget->signal_connect("key_press_event",
				     key_pressed_on_board);
    }


    void new_testcase(array(string) complete_test_, string testcase_command_)
    {
	werror("Loading new testcase.\n");
        worms_initialized = 0;
        dragons_initialized = 0;
        result = "";
        worms = ([]);
        dragons = ([]);

        complete_test = complete_test_;
	testcase_command = testcase_command_;

	load_testcase();
	werror("%s\n", send_command("showboard"));
	int boardsize = (int) send_command("query_boardsize");

	werror("Loaded new testcase.\n");
	goban = Goban(boardsize, 600);
	goban->add_stones("WHITE", send_command("list_stones white") / " ");
	goban->add_stones("BLACK", send_command("list_stones black") / " ");
	redraw_board();
    }


    static void load_testcase()
    {
	foreach(complete_test, string testline) {
	    werror(testline + "\n");
	    if (!has_value("0123456789 #", testline[0..0]))
		send_command(testline);
        }
    }

    void handle_testcase()
    {
	traces = ({});
	engine->trace_callback = collect_traces;
        result = send_command(testcase_command);
	redraw_board();
	engine->trace_callback = 0;
    }

    static void collect_traces(string s)
    {
	traces += ({s});
    }
    
    void get_dragons()
    {
        foreach (send_command("dragon_stones") / "\n", string dragon)
            dragons[(dragon / " ")[0]] = dragon / " " - ({""});
        dragons_initialized = 1;
    }

    static void get_worms()
    {
	foreach (send_command("worm_stones") / "\n", string worm)
	    worms[(worm / " ")[0]] = worm / " " - ({""});
	worms_initialized = 1;
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
            if (!dragons_initialized)
                get_dragons();
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
            if (!dragons_initialized)
                get_dragons();
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
            if (!worms_initialized)
                get_worms();
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
	    parent->delta_territory_button
		  ->set_label("delta territory for "
			      + parent->delta_territory_move);
	    array(string) text_lines = ({});

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
		text_lines += ({traces[k]});
		for (k--; k >= 0; k--)
		{
		    if (sscanf(traces[k], "    %*s:   - %*s") < 2)
			break;

		    text_lines = ({traces[k]}) + text_lines;
		    if (sscanf(traces[k],
			       "    %*s:   - %s territory change %s ",
			       string vertex, string value) == 3)
		    {
			goban->add_text(vertex, value,
					(float) value < 0.0 ? "red" : "blue");
		    }
		}
	    }
	    clist->set_text(text_lines * "\n", -1);
	    parent->set_title(this_object(),
			      ("Delta territory for "
			       + parent->delta_territory_move));
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
	    && parent->first_vertex != "")
	    goban->add_symbol(parent->first_vertex,
	    		      "big_dot", "green");
    }
    
    void redraw_board()
    {
	// Temporary workaround for Pike bug.
#if 0
	gdk_image->set(goban->draw_board());
#else
	gdk_image = GTK2.GdkImage(0)->set(goban->draw_board());
	gtk_image->set_from_image(gdk_image);
#endif
	gtk_image->queue_draw();
    }

    void show_worm_data(string vertex)
    {
	string worm_data = send_command("worm_data " + vertex);
	array(string) text_lines = ({});
	foreach ((worm_data / "\n")[1..], string data_item)
	{
	    sscanf(data_item, "%s%*[ ]%s", string field, string value);
	    text_lines += ({sprintf("%-20s\t%s", field, value)});
	}
	clist->set_text(text_lines * "\n", -1);
	parent->set_title(this_object(), "Worm data for " + vertex);
    }

    void show_dragon_data(string vertex, int part)
    {
	string dragon_data = send_command("dragon_data " + vertex);
	array(string) text_lines = ({});
	
	array(string) selected_data;
	if (part == 1)
	    selected_data = (dragon_data / "\n")[1..20];
	else
	    selected_data = (dragon_data / "\n")[21..];
	
	foreach (selected_data, string data_item)
	{
	    sscanf(data_item, "%s%*[ ]%s", string field, string value);
	    text_lines += ({sprintf("%-20s\t%s", field, value)});
	}
	clist->set_text(text_lines * "\n", -1);
	parent->set_title(this_object(), "Dragon data for " + vertex);
    }

    void show_move_reasons(string vertex)
    {
	string move_reasons = send_command("move_reasons " + vertex);
	array(string) text_lines = ({});

	foreach ((move_reasons / "\n"), string move_reason)
	    text_lines += ({move_reason});

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
	    text_lines += ({""});
	    foreach (reverse(interesting_lines), string line)
		text_lines += ({line});
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
		    text_lines += ({""});
		    first_pattern = 0;
		}
		add_continuation_lines = 1;
		text_lines += ({line});
	    }
	    else if (has_prefix(line, "...") && add_continuation_lines)
		text_lines += ({line});
	    else
		add_continuation_lines = 0;
	}

	/* Look for blunder devaluation */
	foreach (traces, string line) 
	    if (has_prefix(line, "Move at " + vertex + " is"))
		text_lines += ({line});

	clist->set_text(text_lines * "\n", -1);
	parent->set_title(this_object(), "Move reasons for " + vertex);
    }
    
    void show_eye_data(string vertex)
    {
	array(string) text_lines = ({});

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
		text_lines += ({sprintf("%-20s\t%s", field, value)});
	    }
	    if (half_eye_data[vertex])
	    {
		text_lines += ({""});
		foreach (half_eye_data[vertex] / "\n", string data_item)
		{
		    sscanf(data_item, "%s%*[ ]%s", string field, string value);
		    text_lines += ({sprintf("%-20s\t%s", field, value)});
		}
	    }
	}
	
	clist->set_text(text_lines * "\n", -1);
	parent->set_title(this_object(), color + " eye data for " + vertex);
    }

   
    static void button_pressed_on_board(GTK2.Object o, GTK2.GdkEvent event)
    {
//	werror("Button: %f %f\n", event->x[0], event->y[0]);
	string vertex = goban->pixel_coord_to_vertex(event->x[0], event->y[0]);
        on_board_click_callback(vertex);
    }

    static void key_pressed_on_board(GTK2.Object o, GTK2.GdkEvent event)
    {
//	werror("Key: %O\n", event);
	// First thing to find out is what the event object contains and
	// how we can get the mouse position when the key was pressed.
    }
    
    string send_command(string command)
    {
	string result;
	result = engine->send_command(command)->text;
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
	    	    string sgffilename, string sgf_viewer_cmd,
    		    string first_command, string second_command)
    {
	string result;
	send_command("clear_cache");
        if (sizeof(parent->viewers) > 1)
            sgffilename += "." + replace(name, " ", "_");
	if (sgffilename != "")
	    send_command("start_sgftrace");

	send_command(reset_counter);
	result = send_command(first_command);
	array(string) text_lines = ({});
	text_lines += ({first_command + "\t" + result
			+ "\t(" + send_command(get_counter) + " nodes)"});
	
	if (result[0..0] != "0" && second_command != "")
	{
	    send_command(reset_counter);
	    string result = send_command(second_command);
	    text_lines += ({second_command + "\t" + result
			    + "\t(" + send_command(get_counter) + " nodes)"});
	}
	if (sgffilename != "")
	    send_command("finish_sgftrace " + sgffilename);
        if (sgf_viewer_cmd != "")
            Process.create_process(sprintf(sgf_viewer_cmd, sgffilename)
                                   / " ");
	clist->set_text(text_lines * "\n", -1);
	parent->set_title(this_object(), "Reading result");
     }
}


class Controller
{
    array(RegressionViewer) viewers = ({});
    array(GTK2.Widget) viewer_title_widgets = ({});

    string current_move_color = "";
    
    GTK2.Window main_window;
    GTK2.Notebook controller_notebook;
    GTK2.Notebook gobans_notebook;
    GTK2.Notebook data_notebook;
    GTK2.Notebook selector_notebook;

    GTK2.ScrolledWindow scrolled_testcase_text;
    GTK2.Label testcase_text;

    GTK2.RadioButton worm_data_button;
    GTK2.RadioButton dragon_data1_button;
    GTK2.RadioButton dragon_data2_button;
    GTK2.RadioButton worm_status_button;
    GTK2.RadioButton dragon_status_button;
    GTK2.RadioButton dragon_safety_button;
    
    GTK2.RadioButton top_moves_button;
    GTK2.RadioButton all_moves_button;
    GTK2.RadioButton delta_territory_button;
    
//    GTK2.RadioButton initial_w_influence_dragons_unknown_button;
//    GTK2.RadioButton initial_b_influence_dragons_unknown_button;
    GTK2.RadioButton initial_w_influence_dragons_known_button;
    GTK2.RadioButton initial_b_influence_dragons_known_button;
    GTK2.RadioButton after_move_influence_button;
    GTK2.RadioButton followup_influence_button;
    GTK2.RadioButton influence_regions_button;
    GTK2.RadioButton territory_value_button;
    GTK2.RadioButton white_influence_button;
    GTK2.RadioButton black_influence_button;
    GTK2.RadioButton white_strength_button;
    GTK2.RadioButton black_strength_button;
    GTK2.RadioButton white_permeability_button;
    GTK2.RadioButton black_permeability_button;
    GTK2.RadioButton white_attenuation_button;
    GTK2.RadioButton black_attenuation_button;
    GTK2.RadioButton non_territory_button;

    GTK2.RadioButton white_eyes_button;
    GTK2.RadioButton black_eyes_button;
    
    GTK2.RadioButton tactical_reading_button, owl_reading_button,
	owl_does_attack_button, owl_does_defend_button,
	connection_reading_button, semeai_reading_button;
    GTK2.CheckButton sgf_traces_button, sgf_viewer_button;
    GTK2.Entry sgf_filename_entry, sgf_viewer_entry;
    GTK2.Table sgf_stuff;
    GTK2.Button new_testcase_button, new_engine_button;
    GTK2.Button next_testcase_button, prev_testcase_button;
    GTK2.Entry new_testcase_entry, engine_path_entry, engine_name_entry;
    
    string delta_territory_move = "PASS";
    string move_influence_move = "PASS";
    string first_vertex = "";

    int testcase_index = 0;

    array(string) testcases;
    string testcase_name;
    string testcase_command;
    string result;
    string expected_result;

    // All lines from the test file shown at the top of the control window.
    array(string) full_testcase;

    // All lines from the test file which may be needed to load the
    // testcase correctly.
    array(string) complete_testcase;

    static mixed nasty_signal_id;

    static void create(SimpleGtp engine_, array(string) testcases_)
    {
	testcases = testcases_;
	if (!excerpt_testcase(testcases[0], engine_))
	{
	    werror("Failed to load testcase.\n");
	    exit(1);
	}
	testcase_name = testcases[0];

	scrolled_testcase_text
	    = GTK2.ScrolledWindow(GTK2.Adjustment(), GTK2.Adjustment())
		->set_policy(GTK2.POLICY_AUTOMATIC, GTK2.POLICY_AUTOMATIC)
	        ->set_size_request(450, 100);
        testcase_text = GTK2.Label(full_testcase * "\n")
	                          ->set_justify(GTK2.JUSTIFY_LEFT)
			          ->set_alignment(0.0, 0.0);
 	scrolled_testcase_text->add(testcase_text);

	main_window = GTK2.Window(GTK2.WindowToplevel);
	controller_notebook = GTK2.Notebook();
	controller_notebook->set_tab_pos(GTK2.POS_LEFT);

	gobans_notebook   = GTK2.Notebook()->set_show_tabs(0);
	data_notebook     = GTK2.Notebook()->set_show_tabs(0)
	                                   ->set_show_border(0);
	selector_notebook = GTK2.Notebook()->set_tab_pos(GTK2.POS_TOP);
	selector_notebook->signal_connect("switch_page", change_engine);

	GTK2.Widget main_window_contents
	    = (GTK2.Vbox(0, 0)
	       ->pack_start(GTK2.Hbox(0, 24)
			    ->pack_start(scrolled_testcase_text, 0, 0, 24)
			    ->pack_start(GTK2.Alignment(0.0, 0.5, 0.0, 0.0)
					 ->add(selector_notebook),
					 0, 0, 0),
			    0, 0, 0)
	       ->add(GTK2.Hbox(0, 2)
		     ->add(GTK2.Vbox(0, 6)
			   ->pack_start(controller_notebook, 0, 0, 0)
			   ->add(data_notebook))
		     ->pack_start(GTK2.Alignment(0.0, 0.0, 0.0, 0.0)
				  ->add(gobans_notebook),
				  0, 0, 0)));
	main_window->add(main_window_contents);

	main_window->set_title(testcases[0]);
	main_window->signal_connect("destroy", quit);

	if (has_prefix(testcase_command, "reg_genmove")
	    || has_prefix(testcase_command, "restricted_genmove"))
	{
	    sscanf(testcase_command, "%*s %s", string color);
	    if (lower_case(color[0..0]) == "w")
		current_move_color = "white";
	    else
		current_move_color = "black";
	}

	worm_data_button = GTK2.RadioButton("worm data");
	dragon_data1_button = GTK2.RadioButton("dragon data, part 1",
					       worm_data_button);
	dragon_data2_button = GTK2.RadioButton("dragon data, part 2",
					       worm_data_button);

	worm_status_button = GTK2.RadioButton("worm status");
	dragon_status_button = GTK2.RadioButton("dragon status",
						worm_status_button);
	dragon_safety_button = GTK2.RadioButton("dragon safety",
						worm_status_button);
	worm_status_button->signal_connect("clicked", markup_button_pressed);
	dragon_status_button->signal_connect("clicked", markup_button_pressed);
	dragon_safety_button->signal_connect("clicked", markup_button_pressed);

	top_moves_button = GTK2.RadioButton("top moves");
	all_moves_button = GTK2.RadioButton("all moves", top_moves_button);
	delta_territory_button = GTK2.RadioButton("delta territory for PASS",
						  top_moves_button);
	delta_territory_button->set_alignment(0.0, 0.0);

	top_moves_button->signal_connect("clicked", markup_button_pressed);
	all_moves_button->signal_connect("clicked", markup_button_pressed);
	delta_territory_button->signal_connect("clicked",
					       markup_button_pressed);

	white_eyes_button = GTK2.RadioButton("white eyes");
	black_eyes_button = GTK2.RadioButton("black eyes", white_eyes_button);
	white_eyes_button->signal_connect("clicked", markup_button_pressed);
	black_eyes_button->signal_connect("clicked", markup_button_pressed);
	
//	  initial_w_influence_dragons_unknown_button =
//	      GTK2.RadioButton("white influence, dragons unknown");
//	  initial_b_influence_dragons_unknown_button =
//	      GTK2.RadioButton("black influence, dragons unknown",
//			      initial_w_influence_dragons_unknown_button);
	initial_w_influence_dragons_known_button =
	    GTK2.RadioButton("white influence, dragons known");
	initial_b_influence_dragons_known_button =
	    GTK2.RadioButton("black influence, dragons known",
			     initial_w_influence_dragons_known_button);
	after_move_influence_button =
	    GTK2.RadioButton("after move influence for PASS",
			     initial_w_influence_dragons_known_button);
	after_move_influence_button->set_alignment(0.0, 0.0);
	followup_influence_button =
	    GTK2.RadioButton("followup influence for PASS",
			     initial_w_influence_dragons_known_button);
	followup_influence_button->set_alignment(0.0, 0.0);
	influence_regions_button = GTK2.RadioButton("influence regions");
	territory_value_button = GTK2.RadioButton("territory value",
						  influence_regions_button);
	white_influence_button = GTK2.RadioButton("white influence",
						  influence_regions_button);
	black_influence_button = GTK2.RadioButton("black influence",
						  influence_regions_button);
	white_strength_button = GTK2.RadioButton("white strength",
						 influence_regions_button);
	black_strength_button = GTK2.RadioButton("black strength",
						 influence_regions_button);
	white_permeability_button = GTK2.RadioButton("white permeability",
						     influence_regions_button);
	black_permeability_button = GTK2.RadioButton("black permeability",
						     influence_regions_button);
	white_attenuation_button = GTK2.RadioButton("white attenuation",
						    influence_regions_button);
	black_attenuation_button = GTK2.RadioButton("black attenuation",
						    influence_regions_button);
	non_territory_button = GTK2.RadioButton("non-territory",
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
	  non_territory_button})->signal_connect("clicked",
						 markup_button_pressed);
	
	
	tactical_reading_button = GTK2.RadioButton("tactical reading");
	owl_reading_button = GTK2.RadioButton("owl reading",
					      tactical_reading_button);
        owl_does_attack_button = GTK2.RadioButton("owl_does_attack",
						  tactical_reading_button);
        owl_does_defend_button = GTK2.RadioButton("owl_does_defend",
						  tactical_reading_button);
	connection_reading_button = GTK2.RadioButton("connection reading",
						     tactical_reading_button);
	semeai_reading_button = GTK2.RadioButton("semeai reading",
						 tactical_reading_button);
	sgf_traces_button = (GTK2.CheckButton("save sgf traces to")
			     ->set_active(1));
	sgf_filename_entry = GTK2.Entry();
	sgf_filename_entry->set_text("vars.sgf");
	sgf_filename_entry->set_editable(1);

	sgf_viewer_button = GTK2.CheckButton("start sgf viewer as");
        sgf_viewer_entry = GTK2.Entry()->set_text(sgf_viewer_command)
                                       ->set_editable(1);
        sgf_viewer_button->signal_connect("toggled", sgf_viewer_button_toggled);
        sgf_traces_button->signal_connect("toggled", sgf_traces_button_toggled);
        sgf_stuff = (GTK2.Table(2, 2, 0)
		     ->attach_defaults(sgf_traces_button, 0, 1, 0, 1)
		     ->attach_defaults(sgf_filename_entry, 1, 2, 0, 1)
		     ->attach_defaults(sgf_viewer_button, 0, 1, 1, 2)
		     ->attach_defaults(sgf_viewer_entry, 1, 2, 1, 2));

	new_testcase_entry = GTK2.Entry();
        new_testcase_entry->set_text(testcases[0]);
        new_testcase_entry->set_editable(1);
        new_testcase_button = GTK2.Button("Load new testcase");
        new_testcase_button->signal_connect("clicked", new_testcase);
	new_testcase_entry->signal_connect("activate", new_testcase);
	if (sizeof(testcases)) {
	    prev_testcase_button = GTK2.Button("Previous testcase");
	    prev_testcase_button->signal_connect("clicked", prev_testcase);
	    prev_testcase_button->set_sensitive(0);
	    next_testcase_button = GTK2.Button("Next testcase");
	    next_testcase_button->signal_connect("clicked", next_testcase);
	}
	engine_path_entry = GTK2.Entry();
	engine_path_entry->set_text("../interface/gnugo");
	engine_path_entry->set_editable(1);

	engine_name_entry = GTK2.Entry();
	engine_name_entry->set_text("Engine 2");
	engine_name_entry->set_editable(1);

	engine_path_entry->signal_connect("activate", select_new_engine);
	new_engine_button = GTK2.Button("Start new engine");
	new_engine_button->signal_connect("clicked", select_new_engine);

	GTK2.Widget worms_and_dragons_page
	    = (GTK2.Vbox(0, 0)
	       ->pack_start(worm_data_button, 0, 0, 0)
	       ->pack_start(dragon_data1_button, 0, 0, 0)
	       ->pack_start(dragon_data2_button, 0, 0, 0)
	       ->pack_start(GTK2.Label(""), 0, 0, 0)
	       ->pack_start(worm_status_button, 0, 0, 0)
	       ->pack_start(dragon_status_button, 0, 0, 0)
	       ->pack_start(dragon_safety_button, 0, 0, 0));
	controller_notebook->append_page(worms_and_dragons_page,
					 GTK2.Label("worms and dragons"));

	GTK2.Widget move_generation_page
	    = (GTK2.Vbox(0, 0)
	       ->pack_start(top_moves_button, 0, 0, 0)
	       ->pack_start(all_moves_button, 0, 0, 0)
	       ->pack_start(delta_territory_button, 0, 0, 0));
	controller_notebook->append_page(move_generation_page,
					 GTK2.Label("move generation"));

	GTK2.Widget eyes_page = (GTK2.Vbox(0, 0)
				 ->pack_start(white_eyes_button, 0, 0, 0)
				 ->pack_start(black_eyes_button, 0, 0, 0));
	controller_notebook->append_page(eyes_page, GTK2.Label("eyes"));

	GTK2.Widget influence_page
	    = (GTK2.Vbox(0, 0)
	       ->pack_start(GTK2.Vbox(0,0)
// 			    ->add(initial_w_influence_dragons_unknown_button)
// 			    ->add(initial_b_influence_dragons_unknown_button)
			    ->add(initial_w_influence_dragons_known_button)
			    ->add(initial_b_influence_dragons_known_button)
			    ->add(after_move_influence_button)
			    ->add(followup_influence_button), 0, 0, 0)
	       ->pack_start(GTK2.Label(""), 0, 0, 0)
	       ->pack_start(GTK2.Hbox(0,12)
			    ->add(GTK2.Vbox(0,0)
				  ->pack_start(influence_regions_button, 0, 0, 0)
				  ->pack_start(territory_value_button, 0, 0, 0)
				  ->pack_start(non_territory_button, 0, 0, 0)
				  ->pack_start(white_influence_button, 0, 0, 0)
				  ->pack_start(black_influence_button, 0, 0, 0))
			    ->add(GTK2.Vbox(0,0)
				  ->pack_start(white_strength_button, 0, 0, 0)
				  ->pack_start(black_strength_button, 0, 0, 0)
				  ->pack_start(white_permeability_button, 0, 0, 0)
				  ->pack_start(black_permeability_button, 0, 0, 0)
				  ->pack_start(white_attenuation_button, 0, 0, 0)
				  ->pack_start(black_attenuation_button, 0, 0, 0)),
			    0, 0, 0));
	controller_notebook->append_page(influence_page,
					 GTK2.Label("influence"));

	GTK2.Widget reading_page
	    = (GTK2.Vbox(0, 0)
	       ->pack_start(tactical_reading_button, 0, 0, 0)
	       ->pack_start(owl_reading_button, 0, 0, 0)
	       ->pack_start(owl_does_attack_button, 0, 0, 0)
               ->pack_start(owl_does_defend_button, 0, 0, 0)
	       ->pack_start(connection_reading_button, 0, 0, 0)
	       ->pack_start(semeai_reading_button, 0, 0, 0)
	       ->pack_start(GTK2.Label(""), 0, 0, 0)
	       ->pack_start(sgf_stuff, 0, 0, 0));
	controller_notebook->append_page(reading_page, GTK2.Label("reading"));

	GTK2.Widget engines_page
	    = (GTK2.Vbox(0, 12)
	       ->pack_start(engine_path_entry, 0, 0, 0));
	engines_page->pack_start(engine_name_entry, 0, 0, 0);
	engines_page->pack_start(GTK2.Alignment(1.0, 0.0, 0.0, 0.0)
				 ->add(new_engine_button), 0, 0, 0)
		     ->pack_start(new_testcase_entry, 0, 0, 0)
		     ->pack_start(new_testcase_button, 0, 0, 0);
	if  (sizeof(testcases) > 1) {
	    GTK2.Widget next_prev
		= (GTK2.Hbox(0, 12)->pack_start(prev_testcase_button, 0, 0, 0)
				   ->pack_start(next_testcase_button, 0, 0, 0));
	    engines_page->pack_start(next_prev, 0, 0, 0);
	}
	controller_notebook->append_page(engines_page->set_border_width(12),
					 GTK2.Label("engines"));

	nasty_signal_id = controller_notebook->signal_connect("switch_page",
							      markup_button_pressed);

	if (has_prefix(testcase_command, "reg_genmove")
	    || has_prefix(testcase_command, "restricted_genmove")) {
	    controller_notebook->show_all();
	    controller_notebook->set_current_page(1);
	}

	main_window->show_all();

        add_regression_viewer(RegressionViewer(engine_,
					       complete_testcase,
					       testcase_command,
					       button_pressed_on_a_board,
					       "Default engine",
					       this_object()));
	add_markup(controller_notebook->get_current_page());
    }

    static void select_new_engine()
    {
	string new_engine_path = engine_path_entry->get_text();
	if (!Stdio.is_file(new_engine_path)) {
	    viewers->clist->set_text("The engine path does not point to a file.", -1);
	    return;
	}
	
	SimpleGtp new_engine = SimpleGtp((new_engine_path + " --quiet --mode gtp -w -t -d0x101840") / " ");
    	if (!new_engine)
	    werror("Failed to start new engine.\n");
	else {
	    add_regression_viewer(
		RegressionViewer(new_engine, complete_testcase,
				 testcase_command, button_pressed_on_a_board,
				 engine_name_entry->get_text(),
				 this_object()));
	}

	engine_name_entry->set_text(sprintf("Engine %d", sizeof(viewers) + 1));
    }

    static void add_regression_viewer(RegressionViewer viewer)
    {
	viewers += ({ viewer });
	viewer->goban_widget->show_all();
	gobans_notebook->append_page(viewer->goban_widget, 0);

	GTK2.Widget title_label = GTK2.Label("");
	viewer_title_widgets += ({ title_label });

	GTK2.Widget data_page = (GTK2.Vbox(0, 2)
				->pack_start(title_label, 0, 0, 0)
				->add(viewer->scrolled_data_window)
				->show_all());
	data_notebook->append_page(data_page, 0);

	selector_notebook
	    ->append_page((GTK2.Alignment(0.0, 0.5, 0.0, 0.0)
			   ->set_border_width(4)
			   ->add(GTK2.Label(viewer->engine->command_line))),
			  GTK2.Label(viewer->name));
	selector_notebook->show_all();
	selector_notebook->set_current_page(sizeof(viewers) - 1);
    }

    void set_title(RegressionViewer viewer, string title)
    {
	for (int k = 0; k < sizeof(viewers); k++) {
	    if (viewers[k] == viewer)
		viewer_title_widgets[k]->set_text(title);
	}
    }

    static void markup_button_pressed(mixed ... foo)
    {
	add_markup(controller_notebook->get_current_page());
    }

    static void add_markup(int mode)
    {
	viewers->add_markup(mode);
    }

    static void change_engine(int engine_index)
    {
	gobans_notebook->set_current_page(engine_index);
	data_notebook->set_current_page(engine_index);
    }
    
    void button_pressed_on_a_board(string vertex)
    {
	if (vertex == "")
	    return;

	switch (controller_notebook->get_current_page()) {
	case 0:
	    // Worms and dragons.
	    if (worm_data_button->get_active())
		viewers->show_worm_data(vertex);
	    else if (dragon_data1_button->get_active())
		viewers->show_dragon_data(vertex, 1);
	    else
		viewers->show_dragon_data(vertex, 2);
	    break;

	case 1:
	    // Move generation.
	    if (!delta_territory_button->get_active())
		viewers->show_move_reasons(vertex);
	    else
	    {
		delta_territory_move = vertex;
		markup_button_pressed();
	    }
	    break;

	case 2:
	    // Eyes.
	    viewers->show_eye_data(vertex);
	    break;

	case 3:
	    // Influence.
	    if (after_move_influence_button->get_active()
		|| followup_influence_button->get_active())
	    {
		move_influence_move = vertex;
		after_move_influence_button
		    ->set_label("after move influence for " + vertex);
		followup_influence_button
		    ->set_label("followup influence for " + vertex);
		markup_button_pressed();
	    }
	    break;

	case 4:
	    // Reading.
	    string sgffilename;
	    string sgf_viewer_cmd;
	    string reset_counter, get_counter;
	    
            if (sgf_viewer_button->get_active()) {
                sgffilename = sgf_filename_entry->get_text();
                sgf_viewer_cmd = sgf_viewer_entry->get_text();
            }
	    else if (sgf_traces_button->get_active()) {
	    	sgffilename = sgf_filename_entry->get_text();
                sgf_viewer_cmd = "";
	    }
	    else {
	    	sgffilename = "";
                sgf_viewer_cmd = "";
	    }

	    if (first_vertex == ""
		|| tactical_reading_button->get_active()
		|| owl_reading_button->get_active())
		viewers->goban->clear_markup();
		
	    viewers->goban->add_symbol(vertex, "big_dot", "green");
	    viewers->redraw_board();

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
		
		viewers->do_reading(reset_counter, get_counter,
				    sgffilename, sgf_viewer_cmd,
				    prefix + "attack " + vertex,
				    prefix + "defend " + vertex);
	    }
	    else if (owl_does_attack_button->get_active()
                     || owl_does_defend_button->get_active()
		     || connection_reading_button->get_active()
		     || semeai_reading_button->get_active())
	    {
		if (first_vertex == "")
		{
		    first_vertex = vertex;
		}
		else if (first_vertex != vertex)
		{
		    string c1, c2;
		    
                    if (owl_does_attack_button->get_active()) {
                        c1 = sprintf("owl_does_attack %s %s\n",
                                     first_vertex, vertex);
                        viewers->do_reading("reset_owl_node_counter",
                                            "get_owl_node_counter",
                                            sgffilename, sgf_viewer_cmd,
                                            c1, "");
                    }
                    else if (owl_does_defend_button->get_active()) {
                        c1 = sprintf("owl_does_defend %s %s\n",
                                     first_vertex, vertex);
                        viewers->do_reading("reset_owl_node_counter",
                                            "get_owl_node_counter",
                                            sgffilename, sgf_viewer_cmd,
                                            c1, "");
                    }
		    else if (connection_reading_button->get_active())
		    {
			c1 = sprintf("connect %s %s\n",
				    first_vertex,
				    vertex);
			c2 = "dis" + c1;
			viewers->do_reading("reset_connection_node_counter",
					    "get_connection_node_counter",
					    sgffilename, sgf_viewer_cmd,
					    c1, c2);
		    }
		    else
		    {
			c1 = sprintf("analyze_semeai %s %s",
				     first_vertex, vertex);
			c2 = sprintf("analyze_semeai %s %s", vertex,
				     first_vertex);
			// FIXME: We should use a semeai node counter rather
			// than the owl node counter, except that it doesn't
			// exist yet.
			viewers->do_reading("reset_owl_node_counter",
					    "get_owl_node_counter",
					    sgffilename, sgf_viewer_cmd,
					    c1, c2);
		    }
		    first_vertex = "";
		}
	    }
	    break;
	}
    }

    static void this_new_testcase(string new_testcase)
    {
        werror("Trying to load new testcase %s.", new_testcase);
        if (!excerpt_testcase(new_testcase, viewers[0]->engine))
        {
            werror("Failed to load testcase.\n");
            return;
        }
        testcase_name = new_testcase;
	main_window->set_title(testcase_name);
	testcase_text->set_text(full_testcase * "\n");
        viewers->new_testcase(complete_testcase, testcase_command);
        viewers->handle_testcase();

	if (has_prefix(testcase_command, "reg_genmove")
	    || has_prefix(testcase_command, "restricted_genmove")) {
	    controller_notebook->show_all();
	    controller_notebook->set_current_page(1);
	}
    }

    static void new_testcase()
    {
        this_new_testcase(new_testcase_entry->get_text());
    }

    static void next_testcase()
    {
    	if (testcase_index >= sizeof(testcases) - 1)
	    return;
    	testcase_index += 1;
	prev_testcase_button->set_sensitive(1);
	if (testcase_index == sizeof(testcases) - 1)
	    next_testcase_button->set_sensitive(0);
	this_new_testcase(testcases[testcase_index]);
    }

    static void prev_testcase()
    {
    	if (testcase_index == 0)
	    return;
    	testcase_index -= 1;
	if (!testcases)
	    werror("Error handling list of test cases!\n");
	next_testcase_button->set_sensitive(1);
	if (testcase_index == 0)
	    prev_testcase_button->set_sensitive(0);
	this_new_testcase(testcases[testcase_index]);
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

    void sgf_traces_button_toggled()
    {
        if (!sgf_traces_button->get_active())
            sgf_viewer_button->set_active(0);
    }

    void sgf_viewer_button_toggled()
    {
        if (sgf_viewer_button->get_active())
            sgf_traces_button->set_active(1);
    }
    
    void debug_callback(mixed ... args)
    {
	write("Debug callback:%O\n", args);
    }

    static void quit()
    {
	// Otherwise Pike errors occur.
	controller_notebook->signal_disconnect(nasty_signal_id);
	GTK2.main_quit();
    }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 4
 * End:
 */
