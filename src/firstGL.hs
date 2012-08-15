import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.IORef --for the IORef state stuff
import Control.Monad --for stuff like  liftM and unless
import Data.Array.MArray --for the array operations (like newListArray)
import Data.Array.Storable --for some of the Ptr stuff
import Foreign.Ptr --for more of the pointer stuff
import Foreign.Storable (sizeOf)--for sizeOf

data Resources = Resources {vertexArrayVBO :: BufferObject,
                            elementBufferVBO :: BufferObject}

-- function which simply returns the GLfloat array of raw vertex data
rawVertexData :: [GLfloat]
rawVertexData = [-1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 0.0]

-- function which simply returns the GLfloat array of raw color data
rawColorData :: [GLfloat]
rawColorData = [1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 1.0]

-- genObjectNames is _____
-- bindBuffer is a function bindBuffer :: BufferTarget -> ionno something which
--      basically lets you set that type of buffer to something?
-- newListArray makes an array from a list, taking an argument (i, j) where i is
--      the lower bound of indices and j is the upper bound (this array goes
--      from 0 to length)
--
-- a really cool, elegant solution to finding size of the pointer, found on
--      interwebs (http://fhtr.blogspot.com/2009/06/haskell-opengl-utilities.html)
--where ptrsize [] = toEnum 0
--          ptrsize x:xs = toEnum $ length elems * (sizeOf x)
initBuffer :: [GLfloat] -> IO BufferObject
initBuffer rawData = genObjectNames 1 >>= 
        \x@(vbo:t) -> bindBuffer ArrayBuffer $= Just vbo >> 
            newListArray (0, length rawData - 1) rawData >>=
            \arr -> withStorableArray arr (\ptr ->
                bufferData ArrayBuffer $= (toEnum $ length rawData * (sizeOf $ head rawData), ptr, StaticDraw)) >> 
            bindBuffer ArrayBuffer $= Nothing >>
            return vbo

-- builds Resources by calling initBuffer on the vertex and color data
initGeometry :: IO Resources
initGeometry = liftM2 Resources (initBuffer rawVertexData) (initBuffer rawColorData)

render :: Resources -> IO Bool
render res = GL.clear [GL.ColorBuffer] >>
        bindBuffer ArrayBuffer $= Just arr >>
        drawArrays Triangles (fst arrBounds) (read . show . snd $ arrBounds :: NumArrayIndices) >>
        return True
        where
            arr = vertexArrayVBO res
            arrBounds = (0, length rawVertexData - 1)

rendMain :: IO Bool
rendMain =
        -- set a flag to check if the window is closed
        newIORef False >>= \quit -> 

        -- set up the window close callback function
        GLFW.windowCloseCallback $= (writeIORef quit True >> 
                                GLFW.closeWindow >> 
                                GLFW.terminate >> 
                                return True) >>

        -- Set the clear color
        GL.clearColor $= Color4 0 0 0 0 >>

        -- Set clearDepth
        GL.clearDepth $= 1.0 >>

        -- enable and set depth function
        -- replaces glEnable(GL_DEPTH_TEST) and glDepthFunc(GL_LEQUAL)
        depthFunc $= Just Lequal >> 
        
        -- initialize geometry
        initGeometry >>= \res ->

        -- start main loop and define it using where clause
        loop res quit
        where
            loop res quit =
                -- Tells GLFW to look for event callbacks
                GLFW.waitEvents >>
                
                -- Actively redraw window
                render res >> GLFW.swapBuffers >>

                -- read if quit has been called or not
                -- if quit is false, continue loop.  otherwise we're done!
                readIORef quit >>= \q -> case q of 
                    False -> loop res quit
                    _ -> return True

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
