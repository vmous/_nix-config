#!/bin/shell

# "Squash" all local commits into the first local commit reusing first
# commit's message.
git_j_squash_to_first_local() {
    UPSTREAM=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null || echo origin/HEAD)
    LAST_REMOTE_ID=$(git merge-base "${UPSTREAM}" HEAD)
    # Check if we actually found a remote ID to avoid the 'ambiguous argument' error
    if [ -z "$LAST_REMOTE_ID" ]; then
        echo "Error: Could not determine upstream merge base."
        return 1
    fi

    FIRST_LOCAL_ID=$(git rev-list --reverse "${LAST_REMOTE_ID}"..HEAD | head -n 1)
    if [ -z "$FIRST_LOCAL_ID" ]; then
        echo "No local commits found to squash."
        return 0
    fi

    FIRST_LOCAL_MSG=$(git log -1 --format=%B "${FIRST_LOCAL_ID}")
    git reset --soft "${LAST_REMOTE_ID}"
    git commit -m "${FIRST_LOCAL_MSG}"
}

# Merge all uncommited (new, staged and unstaged) files of your working
# directory into the last local commit. Do this only if at least one
# local-only commit exists.
git_j_uncommited_to_last_local() {
    UPSTREAM=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null || echo origin/HEAD)
    LAST_REMOTE_ID=$(git merge-base "${UPSTREAM}" HEAD)
    LOCAL_COUNT=$(git rev-list --count "${LAST_REMOTE_ID}"..HEAD)
    if [ "${LOCAL_COUNT}" -eq 0 ]; then
        echo "There are no local commits to merge uncommited changes into. Aborting..."
        exit 1
    fi
    LAST_LOCAL_MSG=$(git log -1 --format=%B)
    git add -A
    git commit --amend --no-edit
}

# Function to restore all modified files except a specified list
#
# This workflow restores all the files in the current working
# directory. It gets a list of files as arguments which excludes from
# the files it restores. This workflow helps in cases when you
# accidentally change a lot of files but you cannot restore the whole
# working directory because some of the files contain changes you wish
# to preserve. In that case you need to be more surgical and be able to
# restore only the files you want. For example imagine you have been
# working on a feature that touched 4 files but at some point you
# accidentally execute your project's linter with a faulty configuration
# which in turn touches 150 files of your project; with this workflow
# you can restore all the files except from the 4 files you were working
# on.
git_j_restore_all_but() {
  if [ -z "$@" ]; then
    echo "Usage: git j-restore-all-but <file1> [<file2> ...]"
    return 1
  fi

  all_modified_files_str=$(git diff --name-only --diff-filter=M)
  local all_modified_files_arr=( $(echo "${all_modified_files_str}") )

  files_to_keep=("$@")

  files_to_restore=()

  for file in "${all_modified_files_arr[@]}"; do
    keep=false
    for keep_file in "${files_to_keep[@]}"; do
      if [ "${file}" = "${keep_file}" ]; then
        keep=true
        break
      fi
    done

    if ! ${keep}; then
      files_to_restore+=("{$file}")
    fi
  done

  total_modified=${#all_modified_files_arr[@]}
  to_restore_count=${#files_to_restore[@]}
  to_keep_count=$((total_modified - to_restore_count))

  echo "--- Git Restore-All-But Summary ---"
  echo "Files to keep:     ${to_keep_count}/${total_modified}"
  for file in "${files_to_keep[@]}"; do
    echo " - ${file}"
  done
  echo ""

  echo "Files to restore:  ${to_restore_count}/${total_modified}"
  for file in "${files_to_restore[@]}"; do
    echo "  - ${file}"
  done
  echo ""


  if [ ${to_restore_count} -gt 0 ]; then
    read -p "Do you want to proceed with restoring these files? (yes/no): " confirmation
    if [[ "$confirmation" =~ ^[Yy][Ee][Ss]$ ]]; then
      echo "Proceeding with restore..."
      CMD="git restore ${files_to_restore[@]}"
      echo "Running: ${CMD}"
      eval ${CMD}
      echo "Restore complete."
    else
      echo "Operation cancelled."
    fi
  else
    echo "No files to restore based on your selection."
  fi
}

git_j_add_all_tracked_but() {
  EXCLUDE_FILES=("$@");
  git diff --name-only --diff-filter=MD | while read -r FILE; do
    EXCLUDE=false;
    for EXCL in "${EXCLUDE_FILES[@]}"; do
      if [[ "$FILE" == "$EXCL" ]]; then
        EXCLUDE=true
        break
      fi
    done
    if ! $EXCLUDE; then
      git add "$FILE";
    fi;
  done;
}
