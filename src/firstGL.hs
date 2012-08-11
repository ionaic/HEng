import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.IORef
import Control.Monad

--initGeometry = 

-- render = do
render = GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        

--rendMain = do 
rendMain =
        -- set up a flag to check if the window is closed
        --quit <- newIORef False
        newIORef False >>= \quit -> 

        -- set up the window close callback function
        GLFW.windowCloseCallback $= (writeIORef quit True >> GLFW.closeWindow >> GLFW.terminate >> return True) >>

        -- Set the clear color
        GL.clearColor $= Color4 0 0 0 0 >>

        -- Set clearDepth
        GL.clearDepth $= 1.0 >>

        -- enable and set depth function
        -- replaces glEnable(GL_DEPTH_TEST) and glDepthFunc(GL_LEQUAL)
        depthFunc $= Just Lequal >> 

        -- start main loop and define it using where clause
        loop quit
        where
            -- loop quit = do
            loop quit =
                -- Tells GLFW to look for event callbacks
                GLFW.waitEvents >>
                
                -- Actively redraw window
                render >> GLFW.swapBuffers >>

                -- read if quit has been called or not
                -- q <- readIORef quit
                readIORef quit >>= \q -> case q of 
                    False -> loop quit
                    _ -> return True
                -- unless quit is true, continue loop.  otherwise we're done!
                -- unless q $ loop quit
                -- unless readIORef quit loop quit

-- main = do
main =
    -- initialize GLFW
    GLFW.initialize >>

    -- open a window 
    GLFW.openWindow (GL.Size 300 300) [GLFW.DisplayAlphaBits 8] GLFW.Window >>

    -- set window title
    GLFW.windowTitle $= "butts" >>
    rendMain >>

    -- in case the loop ends for some other reason
    GLFW.closeWindow >>
    GLFW.terminate
