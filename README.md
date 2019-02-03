# act-r-imagery-models
Two ACT-R models of mental imagery phenomena: Image scanning and mental rotation.

These models work with ACT-R 7.12.10.  To run ACT-R from sources you will also need to have Quicklisp (https://www.quicklisp.org/beta/) installed for your Lisp application.

Installation.
1. Extract the actr7.x.zip file containing the ACT-R code.
2. In the resulting actr7.x folder, create a folder called 'models' and inside that folder create another folder called 'imagery-models'.
3. Place all of the code files into the 'imagery-models' folder.

Running the models

1. In your lisp, load ACT-R the 'load-act-r.lisp' file.
2. Load either the 'rotation-model.lisp' or 'scanning-model.lisp'
3. Run the model using the function (runsim 50) which will run it for 50 simulated participants. 
