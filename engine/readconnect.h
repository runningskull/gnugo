
struct connection_data {
  float distances[BOARDMAX];
  float deltas[BOARDMAX];
  int coming_from[BOARDMAX];
  int vulnerable1[BOARDMAX];
  int vulnerable2[BOARDMAX];
  int queue[BOARDMAX];
  int queue_start;
  int queue_end;
};

void compute_connection_distances(int str, int target, float cutoff,
				  struct connection_data *conn);
void init_connection_data(int color, const char goal[BOARDMAX],
		                     struct connection_data *conn);
void spread_connection_distances(int color, int target,
				 struct connection_data *conn,
    			    	 float cutoff_distance,
				 int speculative);
void print_connection_distances(struct connection_data *conn);
