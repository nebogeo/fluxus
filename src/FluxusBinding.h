#include <guile/gh.h>
#include <fluxus/GraphicsUtils.h>
#include <fluxus/LinePrimitive.h>
#include "AudioCollector.h"
#include "TurtleBuilder.h"
#include "FluxusMain.h"

#ifndef FLUXUS_BINDING
#define FLUXUS_BINDING

static const int AUDIO_BUFFER_SIZE = 512;

class FluxusBinding
{
public:
	FluxusBinding(int w, int h);
	~FluxusBinding();
	
	static FluxusMain *Fluxus;
	static AudioCollector *Audio;
	static TurtleBuilder turtle;
	static string CallbackString;
	static long FrameCount;

	static PolyPrimitive* StaticCube;
	static PolyPrimitive* StaticPlane;
	static PolyPrimitive* StaticSphere;
	static PolyPrimitive* StaticCylinder;
	
	void RegisterProcs();

	static SCM build_cube();
	static SCM build_plane();
	static SCM build_cylinder(SCM s_hsegments, SCM s_rsegments);
	static SCM build_line(SCM start, SCM swidth, SCM end, SCM ewidth);
	static SCM draw_cube();
	static SCM draw_plane();
	static SCM draw_sphere();
	static SCM draw_cylinder();
	static SCM build_sphere(SCM s_hsegments, SCM s_rsegments);
	static SCM key_pressed(SCM s_key);
	static SCM show_axis(SCM s_id);
	static SCM make_light(SCM cam);
	static SCM clear_lights();
	static SCM light_ambient(SCM id, SCM v);
	static SCM light_diffuse(SCM id, SCM v);
	static SCM light_specular(SCM id, SCM v);
	static SCM light_position(SCM id, SCM v);
	static SCM lock_camera(SCM s_ob);
	static SCM destroy(SCM s_name);
	static SCM clear();
	static SCM grab(SCM s_id);
	static SCM ungrab();
	static SCM apply(SCM s_id);
	static SCM opacity(SCM s_opac);
	static SCM shinyness(SCM s_opac);
	static SCM colour(SCM s_vec);
	static SCM specular(SCM s_vec);
	static SCM ambient(SCM s_vec);
	static SCM emissive(SCM s_vec);
	static SCM flux_identity();
	static SCM translate(SCM s_vec);
	static SCM rotate(SCM s_vec);
	static SCM scale(SCM s_vec);
	static SCM parent(SCM s_p);
	static SCM line_width(SCM s_p);
	static SCM hint_solid();
	static SCM hint_wire();
	static SCM hint_normal();
	static SCM hint_points();
	static SCM hint_anti_alias();
	static SCM hint_none();
	static SCM blur(SCM s_blur);
	static SCM push();
	static SCM pop();
	static SCM collisions(SCM s);
	static SCM ground_plane(SCM s_ori, SCM s_off);
	static SCM active_box(SCM s_name);
	static SCM active_cylinder(SCM s_name);
	static SCM active_sphere(SCM s_name);
	static SCM passive_box(SCM s_name);
	static SCM passive_cylinder(SCM s_name);
	static SCM passive_sphere(SCM s_name);
	static SCM build_hinge2joint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge1, SCM s_hinge2);
	static SCM build_balljoint(SCM s_ob1, SCM s_ob2, SCM s_anchor);
	static SCM joint_vel2(SCM s_joint, SCM s_value);
	static SCM joint_fmax2(SCM s_joint, SCM s_value);
	static SCM joint_fmax(SCM s_joint, SCM s_value);
	static SCM joint_histop(SCM s_joint, SCM s_value);
	static SCM joint_lostop(SCM s_joint, SCM s_value);
	static SCM joint_vel(SCM s_joint, SCM s_value);
	static SCM joint_fudge(SCM s_joint, SCM s_value);
	static SCM set_max_physical(SCM s_value);
	static SCM kick(SCM s_obj, SCM s_vec);
	static SCM twist(SCM s_obj, SCM s_vec);
	static SCM srandom();
	static SCM get_harmonic(SCM s_harm);
	static SCM load_texture(SCM s_name);
	static SCM texture(SCM s_id);
	static SCM engine_callback(SCM s_func);
	static SCM ortho();
	static SCM persp();
	static SCM frame();
	static SCM reset_camera();
	static SCM print_scene_graph();
	static SCM make_lifeforms(SCM s_name);
	static SCM add_lifeform(SCM s_name, SCM s_obj);
	static SCM lifeform_avoidance(SCM s_name, SCM s_obj);
	static SCM lifeform_flockcentering(SCM s_name, SCM s_obj);
	static SCM lifeform_scenecentering(SCM s_name, SCM s_obj);
	static SCM lifeform_inertia(SCM s_name, SCM s_obj);
	static SCM lifeform_scenecentre(SCM s_name, SCM s_obj);
	static SCM lifeform_maxspeed(SCM s_name, SCM s_obj);
	static SCM save_frame(SCM s_name);
	static SCM load(SCM s_name);
	static SCM source(SCM s_name);
	static SCM fluxface(SCM s_name, SCM x, SCM y);
	static SCM clear_fluxface();
	static SCM gain(SCM s_gain);
	static SCM backfacecull(SCM s);
	static SCM desiredfps(SCM s);
	static SCM clear_colour(SCM s_vec);
	static SCM clear_frame(SCM s_gain);
	static SCM turtle_prim(SCM type);
	static SCM turtle_vert();
	static SCM turtle_build();
	static SCM turtle_move(SCM dist);
	static SCM turtle_turn(SCM s_vec);
	static SCM turtle_reset();
	static SCM start_framedump(SCM s_name);
	static SCM end_framedump();
	static SCM process(SCM s_wavname);
};
#endif
