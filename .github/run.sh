export PROJECT_NAME="exercises01"
export SCALAFIX=""
export BRANCH_NAME=${GITHUB_HEAD_REF##*/}

if [ $BRANCH_NAME == "exercises02" ]; then
  export PROJECT_NAME="exercises02"
elif [ $BRANCH_NAME == "exercises03" ]; then
  export PROJECT_NAME="exercises03"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises04" ]; then
  export PROJECT_NAME="exercises04"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises05" ]; then
  export PROJECT_NAME="exercises05"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises06" ]; then
  export PROJECT_NAME="exercises06"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises07" ]; then
  export PROJECT_NAME="exercises07"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises08" ]; then
  export PROJECT_NAME="exercises08"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises09" ]; then
  export PROJECT_NAME="exercises09"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises10" ]; then
  export PROJECT_NAME="exercises10"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises11" ]; then
  export PROJECT_NAME="exercises11"
  export SCALAFIX="scalafix"
elif [ $BRANCH_NAME == "exercises12" ]; then
  export PROJECT_NAME="exercises12"
  export SCALAFIX="scalafix"
fi
echo "Running build for $PROJECT_NAME"
sbt "project $PROJECT_NAME" clean scalafmtCheck test:scalafmtCheck compile $SCALAFIX test