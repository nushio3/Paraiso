/*
  given the left and right primitive variables,
  solve the Riemann problem and calculate the primitive variables
  at characteristic velocity u_query
*/

#include <cmath>
#include <iostream>
using namespace std;


const double kGamma = (5.0)/(3.0);
const double kTolerance = 1e-6;

enum WaveType {
  kRarefaction = 0,
  kShock = 1
};

class SpeedJump {
  const double a,b,c,c2,d,d2,p0;
  const WaveType is_shock;
public:
  SpeedJump(const double &density, const double &pressure, const double &soundspeed,
            const WaveType &w) :
    a(2.0f / (kGamma+1.0f) / (density) ),
    b((kGamma-1.0f)/(kGamma+1.0f)*pressure),
    c(2.0f*soundspeed/(kGamma-1.0f)),
    c2(1.0f / (density * soundspeed)),
    d((kGamma-1.0f)/(2.0f*kGamma)),
    d2(-(kGamma+1.0f)/(2.0f*kGamma)),
    p0(pressure),
    is_shock(w) {}
  double f(const double& p) { // eq (4.6)
    return is_shock
      ? (p-p0) * sqrt(a/(p+b))     // (4.21)
      : c * (pow(p/p0, d) - 1.0f); // (4.26)
  }
  double df_dp(const double& p) { // derivative of f
    return is_shock
      ? (1 - (p-p0) / 2.0f / (b+p)) * sqrt(a/(p+b))
      : c2 * (pow(p/p0, d2));
  }
};

// calculate soundspeed for given density and pressure
double soundspeed (double density, double pressure) {
  return sqrt(kGamma * pressure / density);
}


/* calculate the hydrodynamic variables along x = ut within rarefaction
 eq (4.56) */
void left_rarefaction
(const double& left_density, const double& left_velocity, const double& left_pressure, 
 const double& left_soundspeed, const double& u,
 double& fan_density, double& fan_velocity, double& fan_pressure) {
  const double factor =
    2.0f/(kGamma+1.0f) + (kGamma-1.0f)/(kGamma+1.0f)/left_soundspeed *
    (left_velocity - u);
  fan_density = left_density * pow(factor, 2.0f/(kGamma-1.0f));
  fan_pressure = left_pressure * pow(factor, 2.0f*kGamma/(kGamma-1.0f));
  fan_velocity = 2.0f/(kGamma+1.0f)*
    (left_soundspeed + (kGamma-1.0f)/2.0f*left_velocity + u);
}

/* eq (4.63) */
void right_rarefaction
(const double& right_density, const double& right_velocity, const double& right_pressure, 
 const double& right_soundspeed, const double& u,
 double& fan_density, double& fan_velocity, double& fan_pressure) {
  const double factor =
    2.0f/(kGamma+1.0f) - (kGamma-1.0f)/(kGamma+1.0f)/right_soundspeed *
    (right_velocity - u);
  fan_density = right_density * pow(factor, 2.0f/(kGamma-1.0f));
  fan_pressure = right_pressure * pow(factor, 2.0f*kGamma/(kGamma-1.0f));
  fan_velocity = 2.0f/(kGamma+1.0f)*
    (-right_soundspeed + (kGamma-1.0f)/2.0f*right_velocity + u);
}

/*
  ** Input **
  left_density,  left_velocity,  left_pressure    : left variables
  right_density,  right_velocity,  right_pressure : right variables

  u_query : the velocity in question
  
  ** Output**
  ret_density, ret_velocity, ret_pressure : 
 */
void solve_riemann_1d
(const double left_density, const double left_velocity, const double left_pressure,
 const double right_density, const double right_velocity, const double right_pressure,
 const double u_query,
 double& ret_density, double& ret_velocity, double& ret_pressure) {
  int limitter = 0;

  const double left_soundspeed = soundspeed(left_density, left_pressure);
  const double right_soundspeed = soundspeed(right_density, right_pressure);

  /* whether the left wave is a shock or rarefaction
   c.f Hachisu book, eq (7.52) and so on.*/
  const WaveType left_wave =
    left_velocity - left_soundspeed >
    right_velocity - right_soundspeed
    ? kShock
    : kRarefaction;
  
  /* whether the right wave is a shock or rarefaction */
  const WaveType right_wave =
    left_velocity + left_soundspeed >
    right_velocity + right_soundspeed
    ? kShock
    : kRarefaction;

  const double separating_speed = right_velocity - left_velocity;

  /*values to be pre-calculated*/
  bool is_vacuum_generated;
  double left_head_speed, left_tail_speed, right_head_speed, right_tail_speed;
  double star_pressure, star_speed, left_star_density, right_star_density;

  /* test if the vacuum is generated */
  const double left_vacuum_front_speed = left_velocity + 
    2.0f * left_soundspeed / (kGamma - 1.0f);
  double right_vacuum_front_speed = right_velocity -
    2.0f * right_soundspeed / (kGamma - 1.0f);
  double critical_speed = 
    left_vacuum_front_speed - right_vacuum_front_speed;

  is_vacuum_generated = (critical_speed < 0.0f);
  if (is_vacuum_generated) {
    left_head_speed = left_velocity - left_soundspeed;
    left_tail_speed = left_vacuum_front_speed;
    right_head_speed = right_velocity + right_soundspeed;
    right_tail_speed = right_vacuum_front_speed;
  } else {
    /* Objects that calculate f_L, f_R and their derivatives */
    SpeedJump left_jump(left_density, left_pressure, left_soundspeed,left_wave);
    SpeedJump right_jump(right_density, right_pressure, right_soundspeed,right_wave);

    /* calculate star_pressure_ by Neuton Raphson method
       c.f. Toro section 4.3.2 */
    {
      double p_improved_prediction;
      double p_cand = p_improved_prediction =
        powf(
            (left_soundspeed + right_soundspeed -
             0.5f*(kGamma-1)*separating_speed) /
            (left_soundspeed / powf(left_pressure, (kGamma-1.0f)/(2.0f*kGamma)) +
             right_soundspeed / powf(right_pressure, (kGamma-1.0f)/(2.0f*kGamma)) )
            ,2.0f*kGamma / (kGamma-1.0f));
      for(limitter = 0; limitter < 20; ++limitter) {
        const double f = separating_speed +
          left_jump.f(p_cand) + right_jump.f(p_cand);
        const double df_dp = left_jump.df_dp(p_cand) + right_jump.df_dp(p_cand);
        const double p_next = p_cand - f / df_dp;
        const double change = abs(p_next - p_cand) / (0.5f * abs(p_next + p_cand));
        p_cand = p_next;
        if (change < kTolerance) {limitter += 1; break;}

        if (p_cand <= 0.0f) { /* binary search combined with newtonian method */
          p_cand = (p_improved_prediction *= 0.3f);
        }
      }
      star_pressure = p_cand;

      /* calculate star speed; the average of (4.21) and (4.26)*/
      star_speed = 0.5f * (left_velocity + right_velocity) +
        0.5f * (right_jump.f(star_pressure) - left_jump.f(star_pressure));

      /* calculate other star values */
      if (left_wave == kShock) {
        left_star_density = left_density *
          (star_pressure / left_pressure + (kGamma-1.0f)/(kGamma+1.0f)) /
          ((kGamma-1.0f)/(kGamma+1.0f) * star_pressure / left_pressure + 1.0f);
         // (4.19)
        
        left_head_speed = left_tail_speed = left_velocity -
          left_soundspeed * sqrt((kGamma+1.0f)/(2.0f*kGamma) * star_pressure/left_pressure +
				   (kGamma-1.0f)/(2.0f*kGamma));
        // (4.51)
        
      } else { // left_wave_ == kRarefaction
        left_star_density = left_density *
          pow(star_pressure / left_pressure, 1.0f/kGamma); // (4.53)
        left_head_speed = left_velocity - left_soundspeed;
        left_tail_speed = star_speed - soundspeed(left_star_density, star_pressure); //(4.55)
      }
    
      if (right_wave == kShock) {
        right_star_density = right_density *
          (star_pressure / right_pressure + (kGamma-1)/(kGamma+1)) /
          ((kGamma-1)/(kGamma+1) * star_pressure / right_pressure + 1);
        right_head_speed = right_tail_speed = right_velocity +
          right_soundspeed * sqrt((kGamma+1)/(2*kGamma) * star_pressure/right_pressure +
                                    (kGamma-1)/(2*kGamma));
      } else { // right_wave_ == kRarefaction
        right_star_density = right_density *
          pow(star_pressure / right_pressure, 1.0f/kGamma);
        right_head_speed = right_velocity + right_soundspeed;
        right_tail_speed = star_speed + soundspeed(right_star_density, star_pressure);
      }
    }
  } // end of non vacuum-generating case
  
  /* calculate the primitive variables for characteristics 
     with velocity u_query. c.f. Fig 4.14*/
  if (is_vacuum_generated) { 
    if (u_query< left_tail_speed) {
      if (u_query< left_head_speed) {
	ret_density  = left_density;
	ret_velocity = left_velocity;
	ret_pressure = left_pressure;	
        return;
      }
      left_rarefaction(left_density,left_velocity,left_pressure,
		       left_soundspeed, u_query,
		       ret_density, ret_velocity, ret_pressure);
      return;
    } else if (u_query> right_tail_speed) {
      if (u_query> right_head_speed) {
	ret_density  = right_density;
	ret_velocity = right_velocity;
	ret_pressure = right_pressure;
	return;
      }
      right_rarefaction(right_density,right_velocity,right_pressure,
                        right_soundspeed, u_query,
                        ret_density, ret_velocity, ret_pressure);      
      return;
    } else { // u_queryresides inside vaccum
      ret_density=ret_velocity=ret_pressure = 0;
      return;
    }
  }

  /* non vacuum-generating cases */
  if (u_query< star_speed) { // the u_queryis left of contact discontinuity
    if (left_wave == kShock) { // the left wave is a shock
      if (u_query< left_head_speed) { // the u_queryis before the shock
	ret_density  = left_density;
	ret_velocity = left_velocity;
	ret_pressure = left_pressure;
	return;
      } else { // the u_queryis behind the shock
	ret_density  = left_star_density;
	ret_velocity = star_speed; 
	ret_pressure = star_pressure;
	return;
      }
    } else { // the left wave is a rarefaction
      if (u_query< left_head_speed) { // the u_queryis before the rarefaction
	ret_density  = left_density;
	ret_velocity = left_velocity;
	ret_pressure = left_pressure;
	return;
      } else if (u_query< left_tail_speed) { // the u_queryis inside the rarefaction
	left_rarefaction(left_density,left_velocity,left_pressure,
			 left_soundspeed,u_query,
			 ret_density, ret_velocity, ret_pressure);
	return;	
      } else { // the u_queryis after the rarefaction
	ret_density  = left_star_density;
	ret_velocity = star_speed; 
	ret_pressure = star_pressure;
	return;	
      }
    }
  } else { // the queried u_queryis right of contact discontinuity
    if (right_wave == kShock) { // the right wave is a shock
      if (u_query> right_head_speed) { // the u_queryis before the shock
	ret_density  = right_density;
	ret_velocity = right_velocity;
	ret_pressure = right_pressure;
	return;
      } else { // the u_queryis behind the shock
	ret_density  = right_star_density;
	ret_velocity = star_speed; 
	ret_pressure = star_pressure;
	return;
      }
    } else { // the right wave is a rarefaction
      if (u_query> right_head_speed) { // the u_queryis before the rarefaction
	ret_density  = right_density;
	ret_velocity = right_velocity;
	ret_pressure = right_pressure;
	return;
      } else if (u_query> right_tail_speed) { // the u_queryis inside the rarefaction
	right_rarefaction(right_density,right_velocity,right_pressure,
                          right_soundspeed,u_query,
                          ret_density, ret_velocity, ret_pressure);
	return;	
      } else { // the u_queryis after the rarefaction
	ret_density  = right_star_density;
	ret_velocity = star_speed; 
	ret_pressure = star_pressure;
	return;
      }
    }
  }
}
