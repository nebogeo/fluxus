#include <guile/gh.h>
#include <GraphicsUtils.h>
#include <LinePrimitive.h>
#include <set>
#include "AudioCollector.h"
#include "TurtleBuilder.h"
#include "FluxusMain.h"

#ifndef FLUXUS_BINDING
#define FLUXUS_BINDING

static const int AUDIO_BUFFER_SIZE = 2048;

class FluxusBinding
{
public:
	FluxusBinding(int w, int h);
	~FluxusBinding();
	
	static FluxusMain *Fluxus;
	static AudioCollector *Audio;
	static TurtleBuilder turtle;
	static string CallbackString;
	static int GrabbedID;

	static PolyPrimitive* StaticCube;
	static PolyPrimitive* StaticPlane;
	static PolyPrimitive* StaticSphere;
	static PolyPrimitive* StaticCylinder;
	
	static set<int> m_KeySet;
	
	void RegisterProcs();

	static SCM build_cube();
	static SCM build_sphere(SCM s_hsegments, SCM s_rsegments);
	static SCM build_plane();
	static SCM build_cylinder(SCM s_hsegments, SCM s_rsegments);
	static SCM build_line(SCM start, SCM swidth, SCM end, SCM ewidth);
	static SCM build_text(SCM text);
	static SCM build_nurbs_sphere(SCM s_hsegments, SCM s_rsegments);
	static SCM build_nurbs_plane(SCM s_usegments, SCM s_vsegments);
	static SCM draw_instance(SCM s_ob);
	static SCM draw_cube();
	static SCM draw_plane();
	static SCM draw_sphere();
	static SCM draw_cylinder();
	static SCM key_pressed(SCM s_key);
	static SCM show_axis(SCM s_id);
	static SCM show_fps(SCM s_id);
	static SCM make_light(SCM cam);
	static SCM clear_lights();
	static SCM light_ambient(SCM id, SCM v);
	static SCM light_diffuse(SCM id, SCM v);
	static SCM light_specular(SCM id, SCM v);
	static SCM light_position(SCM id, SCM v);
	static SCM lock_camera(SCM s_ob);
	static SCM get_transform();
	static SCM get_camera_transform();
	static SCM destroy(SCM s_name);
	static SCM clear();
	static SCM grab(SCM s_id);
	static SCM ungrab();
	static SCM hide(SCM s_b);
	static SCM apply(SCM s_id);
	static SCM opacity(SCM s_opac);
	static SCM shinyness(SCM s_opac);
	static SCM colour(SCM s_vec);
	static SCM specular(SCM s_vec);
	static SCM ambient(SCM s_vec);
	static SCM emissive(SCM s_vec);
	static SCM flux_identity();
	static SCM concat(SCM s_m);
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
	static SCM hint_unlit();
	static SCM hint_none();
	static SCM blur(SCM s_blur);
	static SCM feedback(SCM s_fb);
	static SCM feedback_transform(SCM s_fb);
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
	static SCM surface_params(SCM s_slip1, SCM s_slip2, SCM s_softerp, SCM s_softcfm);
	static SCM build_hingejoint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge);
	static SCM build_balljoint(SCM s_ob1, SCM s_ob2, SCM s_anchor);
	static SCM build_sliderjoint(SCM s_ob1, SCM s_ob2, SCM s_hinge);
	static SCM build_hinge2joint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge1, SCM s_hinge2);
	static SCM build_amotorjoint(SCM s_ob1, SCM s_ob2, SCM s_axis);
	static SCM joint_param(SCM s_joint, SCM s_param, SCM s_value);
	static SCM joint_angle(SCM s_joint, SCM s_vel, SCM s_angle);
	static SCM set_max_physical(SCM s_value);
	static SCM set_mass(SCM s_obj, SCM s_value);
	static SCM kick(SCM s_obj, SCM s_vec);
	static SCM twist(SCM s_obj, SCM s_vec);
	static SCM gravity(SCM s_vec);
	static SCM srandom();
	static SCM get_harmonic(SCM s_harm);
	static SCM load_texture(SCM s_name);
	static SCM texture(SCM s_id);
	static SCM engine_callback(SCM s_func);
	static SCM ortho();
	static SCM persp();
	static SCM time();
	static SCM delta();
	static SCM reset_camera();
	static SCM print_scene_graph();
	static SCM save_frame(SCM s_name);
	static SCM load(SCM s_name);
	static SCM save_name(SCM s_name);
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
	static SCM start_osc(SCM s_port);
	static SCM from_osc(SCM s_token, SCM s_index);
	static SCM peek_osc();
	static SCM pdata_size();
	static SCM pdata_get(SCM s_t, SCM s_i);
	static SCM pdata_set(SCM s_t, SCM s_i, SCM s_v);
	static SCM finalise();
	static SCM recalc_normals();
		
	static SCM vmul(SCM s_a, SCM s_b);
	static SCM vadd(SCM s_a, SCM s_b);
	static SCM vsub(SCM s_a, SCM s_b);
	static SCM vdiv(SCM s_a, SCM s_b);
	static SCM vtransform(SCM s_v, SCM s_m);
	static SCM vtransform_rot(SCM s_v, SCM s_m);
	static SCM vnormalise(SCM s_v);
	static SCM vdot(SCM s_a, SCM s_b);
	static SCM vmag(SCM s_a);
	static SCM vdist(SCM s_a, SCM s_b);
	static SCM vcross(SCM s_a, SCM s_b);
	static SCM mmul(SCM s_a, SCM s_b);
	static SCM madd(SCM s_a, SCM s_b);
	static SCM msub(SCM s_a, SCM s_b);
	static SCM mdiv(SCM s_a, SCM s_b);
	static SCM mident();
	static SCM mtranslate(SCM s_v);
	static SCM mrotate(SCM s_v);
	static SCM mscale(SCM s_v);
	static SCM mtranspose(SCM s_a);
	static SCM minverse(SCM s_a);
	static SCM maim(SCM s_a, SCM s_b);
	
};
#endif
