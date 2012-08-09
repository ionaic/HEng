#include <GL/glfw.h>
#include <stdlib.h>

void GLFWCALL keyCallback( int key, int action ) {
	if (key==GLFW_KEY_ESC && action==GLFW_PRESS) {
		glfwTerminate();
		exit( EXIT_SUCCESS );
	}
}

int main( void ) {
	// Initialize GLFW
	if( !glfwInit() ) {
		exit( EXIT_FAILURE );
	}
	// Open an OpenGL window
	if( !glfwOpenWindow( 300,400, 8,8,8,8,0,0, GLFW_WINDOW ) ) {
		glfwTerminate();
		exit( EXIT_FAILURE );
	}
	glfwSetKeyCallback(keyCallback);
	// Main loop
	while( glfwGetWindowParam( GLFW_OPENED ) ) {
		// OpenGL rendering goes here...
		glClear( GL_COLOR_BUFFER_BIT );
		// Swap front and back rendering buffers
		glfwSwapBuffers();
	}
	// Close window and terminate GLFW
	glfwTerminate();
	// Exit program
	exit( EXIT_SUCCESS );
}
