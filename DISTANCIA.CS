#include <stdio.h>
#include <Math.h>
#define PI 3.14159265358979323846
 
double Distance(double lat1, double lon1, double lat2, double lon2, char unit) {
    double deg2radMultiplier = PI / 180;
    lat1 = lat1 * deg2radMultiplier;
    lon1 = lon1 * deg2radMultiplier;
    lat2 = lat2 * deg2radMultiplier;
    lon2 = lon2 * deg2radMultiplier;
 
    double radius = 6378.137; // earth mean radius defined by WGS84
    double dlon = lon2 - lon1;
    double distance = acos( sin(lat1) * sin(lat2) +  cos(lat1) * cos(lat2) * cos(dlon)) * radius; 
    
    if (unit == 'T') {
        return (distance * 1000); 
    } else if (unit == 'M') {
        return (distance * 0.621371192);
    } else if (unit == 'N') {
        return (distance * 0.539956803);
    } else {
        return 0;
    }
}