-- Author: Michael Balas

import GraphicSVG exposing (..)
import Array
import Time exposing (Time, second)

type Message = GameTick Float GetKeyState --The tick needs to have Float and GetKeyState which handles key presses.
              | NextSlide
              | LastSlide

main = gameApp GameTick {
                            model = init,
                            view = view,
                            update = update
                        }

-- MODEL

init = {
              t = 0 ,
              idx = 0 ,
              p = False, -- Pause
              r = 1 , -- Rewind
              a = 1  -- Acceleration
        }

-- VIEW

view model = let t = model.t 
                 slide = Maybe.withDefault default (Array.get model.idx slides)

             in collage 1000 500 (slide t ++ borders ++ navigators)

-- UPDATE

update message model =
  case message of
    GameTick tick (getKeyState,changeP1,changeP2) -> 
                              if (getKeyState LeftArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = max (model.idx - 1) 0
                              }
                              else if (getKeyState RightArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = min (model.idx + 1) (Array.length slides - 1)   -- Next Slide
                              }
                              else if (getKeyState Space) == JustDown then
                              { model |
                                  p = not model.p   -- Pause Animation(s)
                              }
                              else if (getKeyState UpArrow) == JustDown then
                              { model |
                                  a = min (model.a * 2) 4   -- Accelerate Animation(s)
                              }
                              else if (getKeyState DownArrow) == JustDown then
                              { model |
                                  a = max (model.a / 2) 0.5   -- Deccelerate Animation(s)
                              }
                              else if (getKeyState (Key "R")) == JustDown then
                              { model |
                                  r = -model.r   -- Reverse Animation(s)
                              }
                              else if (getKeyState Backspace) == JustDown then
                              { model |
                                  t = 0   -- Reset Slide/Animation(s)
                              }
                              else if model.p then
                              model
                              else
                              { model |
                                       t = max (model.t + 2.5 * model.a * model.r) 0
                              }
    NextSlide -> { model |
    t   = 0 ,
    idx = min (model.idx + 1) (Array.length slides - 1) 
  }
    LastSlide -> { model |
    t   = 0 ,
    idx = max (model.idx - 1) 0
  }

--- MISCELLANEOUS

default t = []

borders = [rect 5000 5000
              |> filled white
              |> move (3000,0),
           rect 5000 5000
              |> filled white
              |> move (-3000,0),
           rect 5000 5000
              |> filled white
              |> move (0,2750),
           rect 5000 5000
              |> filled white
              |> move (0,-2750)]

navigators = [ group [ circle 30
                        |> filled gray
                      ,
                      triangle 20
                        |> filled purple

                      ] |> move (450,-200)
                        |> makeTransparent 0.5
                |> notifyTap NextSlide
              ,
              group [ circle 30
                        |> filled gray
                      ,
                      triangle 20
                        |> filled purple

                    ] |> rotate (degrees 180)
                      |> move (-450,-200)
                      |> makeTransparent 0.5
                |> notifyTap LastSlide
            ]


-- FUNCTIONS


disappear x n = if x > n then makeTransparent 0 else makeTransparent 1 
 
loop t n = let y = toFloat (floor (t / n)) 
           in t - y * n                                           

appear x n =    if x > n then makeTransparent 1 else makeTransparent 0 
                                          
fadeIn t n = makeTransparent (tranSin (t-n) 1) 

fadeOut t n = makeTransparent (1 - (tranSin (t-n) 1)) 

trans t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) LINEAR.
               then 0 
            else Basics.min t y

tranSin t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) Uses sin.
               then 0 
            else if t/100 > pi/2 then y
            else sin(t/100) * y

drawLine t (x1,y1) (x2,y2) = line (x1,y1) (x1 + tranSin (t) (x2 - x1), y1 + tranSin (t) (y2 - y1))


-- Total of 6 Slides
slides = Array.fromList [slide1,slide2,slide3,slide4, slide5, slide6]

--<< SLIDE 1>>-

slide1 t = [ 
              rect 1000 1000
                |> filled lightBlue
                ,
             text "The Fast Fourier Transform (FFT)"
                |> size 50
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled white
                |> move (-100,-40),
              text "Left & Right Arrow keys for previous and next slide, respectively."
                |> size 10
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled black
                |> move (0,-160)
                |> fadeIn t 1000
                ,
                text "Up & Down Array keys for speeding up and down, respectively. Space for pause."
                |> size 10
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled black
                |> move (0,-180)
                |> fadeIn t 1000
                ,
             text "Adapted From: Fast Fourier Transform: Algorithms and Applications" 
                |> size 25
                |> customFont "Roboto"
                |> centered
                |> filled white
                |> move (-95,-100)
                |> fadeIn t 700
                ,
            ellipses (loop t  330)   -- Flashing ellipses indicating it's time to move to the next slide
                |> fadeIn t 600 ,
              rect 1000 500
                |> filled black
                |> move (-500 - (tranSin (t-550) 500),0),
              text "HIS FI"
                |> size 40
                |> customFont "Helvetica"
                |> bold
                |> filled white
                |> move (-115 - (tranSin (t-550) 500),30)
                |> appear t 100
               ,
              text "PRESEN"
                |> size 40
                |> customFont "roboto"
                |> bold
                |> filled white
                |> move (-150 - (tranSin (t-550) 500),-20)
                |> appear t 100
               ,
              rect 1000 500
                |> filled white
                |> move (500 + (tranSin (t-550) 500),0),
              text "RST ELM"
                |> size 40
                |> customFont "roboto"
                |> bold
                |> filled black
                |> move (0 + (tranSin (t-550) 500),30)
                |> appear t 100
                ,
              text "TATION"
                |> size 40
                |> customFont "roboto"
                |> bold
                |> filled black
                |> move (0 + (tranSin (t-550) 500),-20)
                |> appear t 100
               ,
             rect 1000 500
                |> filled white
                |> move (0,200 + tranSin(t-300) 300)
                ,
              rect 1000 500
                |> filled black
                |> move (0,-250 - tranSin(t-300) 300), 
              text "MICHAEL"
                |> size 40
                |> customFont "roboto"
                |> bold
                |> filled black
                |> move (-480,45 + tranSin(t-300) 300)
                |> appear t 100
               ,
              text "PRESENTS"
                |> size 40
                |> customFont "roboto"
                |> bold
                |> filled white
                |> move (-480,-60 - tranSin(t-300) 300)
                |> appear t 200
                ]

ellipses t = group [   -- Each circle fades in and out right after the previous
               circle 10
                  |> filled white
                  |> move (300,-25)
                  |> fadeIn t 50 
                  |> fadeOut t 100
              ,   
               circle 10
                  |> filled white
                  |> move(360, -25)
                  |> fadeIn t 100 
                  |> fadeOut t 150
              ,   
               circle 10
                  |> filled white
                  |> move (420, -25)
                  |> fadeIn t 150 
                  |> fadeOut t 200
          ]

--<< SLIDE 2 >>--

slide2 t = [ 
              rect 1000 500
                |> filled lightBlue
                |> move (0,350), 
              text "What Is It?"
                |> size 40
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled white
                |> move (-230,130)
                |> fadeIn t 100
               ,
             group( makeGreyBullets (t-130) 
                          ["Efficient implementation of the Discrete Fourier Transform (DFT) or its inverse",
                           "Many types: Split radix, prime factor, Winograd Fourier transform, vector radix",
                           "Focus will be on the Radix-2 Decimation in time FFT Algorithm"] 0)
                |> move (-120,0)
             ]

--<< SLIDE 3 >>--

slide3 t = [
              rect 1000 500
                |> filled lightBlue
                |> move (500,0), 
              text "What's the Big Deal?"
                |> size 40
                |> customFont "Roboto"
                |> centered
                |> filled black
                |> move (-250,0)
                |> fadeIn t 100
               ,
              text "Included in the Top 10 Algorithms of"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-250,-60)
                |> fadeIn t 140
               ,
              text "the 20th Century by the IEEE journal:"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-250,-90)
                |> fadeIn t 140
               ,
              text "Computing in Science & Engineering"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-250,-120)
                |> fadeIn t 140
               ,
            group( makeWhiteBullets (t-100) 
                          ["EKG and EEG signal processing",
                           "Forensic Science",
                           "Magnetic Resonance Imaging (MRI)",
                           "Video/image compression",
                           "Pattern Recognition",
                           "Noise filtering",
                           "Image quality measures",
                           "Optical signal processing"] 0)
                |> move (240,220)
              
           ]

--<< SLIDE 4 >>--

slide4 t = [
              rect 1000 500
                |> filled lightBlue
                |> move (0,350),  
              text "Time Comparison"
                |> size 40
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled white
                |> move (-230,130)
                |> fadeIn t 100
                ,
                table t   -- "Table" showing time differences between DFT and FFT
                ,
                text "Assuming 1 nanosecond per operation"
                |> size 25
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-80,-200)
                |> fadeIn t 840
                ,
              blackellipses (loop t  330)   -- Black ellipses indicating that it's time for the next slide
                |> fadeIn t 900
                ]

table t = group [ 
               text "Sample Size (N):"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-220,0)
                |> fadeIn t 140
                ,
              text "1000"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-30,0)
                |> fadeIn t 160
                ,
              text "10^6"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (100,0)
                |> fadeIn t 220
                ,
              text "10^9"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (230,0)
                |> fadeIn t 280
                ,
              text "Discrete Fourier Transform (N^2):"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-290,-50)
                |> fadeIn t 340
                ,
              text "10^6"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-30,-50)
                |> fadeIn t 400
                ,
              text "10^12"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (100,-50)
                |> fadeIn t 460
                ,
              text "10^18"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (230,-50)
                |> fadeIn t 520
                ,
              text "Fast Fourier Transform (Nlog2N):"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-290,-100)
                |> fadeIn t 580
                ,
              text "10^4"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (-30,-100)
                |> fadeIn t 640
                ,
              text "20 x 10^6"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (100,-100)
                |> fadeIn t 700
                ,
              text "30 x 10^9"
                |> size 20
                |> customFont "Roboto"
                |> centered
                |> filled darkGrey
                |> move (230,-100)
                |> fadeIn t 760
          ]
blackellipses t = group [ 
               circle 10
                  |> filled black
                  |> move (160,-190)
                  |> fadeIn t 50 
                  |> fadeOut t 100
              ,   
               circle 10
                  |> filled black
                  |> move(220, -190)
                  |> fadeIn t 100 
                  |> fadeOut t 150
              ,   
               circle 10
                  |> filled black
                  |> move (280, -190)
                  |> fadeIn t 150 
                  |> fadeOut t 200
          ]

--<< SLIDE 5 >>--

slide5 t = [ 
              text "DFT: 31.2 years!"
                |> size 30
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled black
                |> move (-275,150)
                |> fadeIn t 100
                ,
              text "FFT: 30 seconds!"
                |> size 30
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled black
                |> move (275,150)
                |> fadeIn t 100
                ,
                circle 100   -- DFT clock (shows that the DFT is slow)
                |> filled red 
                |> move (-275, 0)
                |> fadeIn t 150
                ,
                drawLine (t-150) (0,0) (100 * cos(t/3600),-100 * sin(t/3600))   -- Hour hand on DFT clock
                |> outlined (solid 2) black
                |> move(-275, 0)
                ,
                drawLine (t-150) (0,0) (100 * cos(t/60),-100 * sin(t/60))   -- Minute hand on DFT clock
                |> outlined (solid 1) black
                |> move(-275, 0)
                ,
                circle 100   -- FFT clock (shows that the FFT is fast)
                |> filled lightBlue 
                |> move (275, 0)
                |> fadeIn t 150
                ,
                drawLine (t-150) (0,0) (100 * cos(t/300),-100 * sin(t/300))   -- Hour hand on FFT clock
                |> outlined (solid 2) black
                |> move(275, 0)
                ,
                drawLine (t-150) (0,0) (100 * cos(t/5),-100 * sin(t/5))   -- Minute Hand on FFT clock
                |> outlined (solid 1) black
                |> move(275, 0)
             ]
slide6 t = [ 
              rect 1000 1000
                |> filled lightBlue
                ,
             text "The Fast and the Fourier"   -- Clever pun inserted here
                |> size 50
                |> customFont "Roboto"
                |> bold
                |> centered
                |> filled white
                |> move (-100,-40)
                ,
              text "The End"
                |> size 35
                |> customFont "Roboto"
                |> centered
                |> filled white
                |> move (-295, -120)
                ,
              circle 25   -- White circle that moves around in a circle
                |> filled white 
                |> move (50 * cos(t/30) + 320, -50 * sin(t/30))
                ,
                circle 25   -- Black circle that moves around in a circle
                |> filled black 
                |> move (-50 * cos(t/30) + 320, 50 * sin(t/30))
            ]

-- Widely spaced out dark grey and white bullet points

makeWhiteBullets t l start = case l of
  x::xs -> group [
            text x
              |> size 22
              |> customFont "Roboto"
              |> filled white
              |> move (-200,start)
              |> fadeIn t 100
            ,
            circle 5
              |> filled white
              |> move (-220,start+5)
              |> fadeIn t 100
            ] :: makeWhiteBullets (t-100) xs (start - 65)
  _     -> []

makeGreyBullets t l start = case l of
  x::xs -> group [
            text x
              |> size 22
              |> customFont "Roboto"
              |> filled darkGrey
              |> move (-200,start)
              |> fadeIn t 100
            ,
            circle 5
              |> filled darkGrey
              |> move (-220,start+5)
              |> fadeIn t 100
            ] :: makeGreyBullets (t-100) xs (start - 65)
  _     -> []


darkGrey = rgb 115 115 115
lightBlue = rgb 66 133 244

--< Hope You Enjoyed >--
    -- Michael Balas
